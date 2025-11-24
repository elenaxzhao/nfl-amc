#!/usr/bin/env python3
"""
NFL Team ELO Rating System using Poisson Model
==============================================

This script implements an ELO rating system for NFL teams using a Poisson model
based on their historical performance. The model considers:
- Offensive and defensive scoring rates
- Game outcomes and margins
- Strength of schedule adjustments
- Home field advantage

Author: NFL Bets Analysis
"""

import numpy as np
import pandas as pd
import nflreadpy as nfl
from scipy.optimize import minimize
from scipy.stats import poisson
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime
import warnings
warnings.filterwarnings('ignore')

class PoissonELOModel:
    """
    Poisson-based ELO rating system for NFL teams
    """
    
    def __init__(self, initial_rating=1500, k_factor=32, home_advantage=25):
        """
        Initialize the ELO model
        
        Parameters:
        -----------
        initial_rating : float
            Starting ELO rating for all teams
        k_factor : float
            How much ratings change after each game
        home_advantage : float
            ELO points added for home field advantage
        """
        self.initial_rating = initial_rating
        self.k_factor = k_factor
        self.home_advantage = home_advantage
        self.team_ratings = {}
        self.offensive_rates = {}
        self.defensive_rates = {}
        
    def load_nfl_data(self, years=[2024]):
        """
        Load NFL play-by-play and schedule data
        
        Parameters:
        -----------
        years : list
            Years to load data for
        """
        print("Loading NFL data...")
        
        try:
            # Load play-by-play data
            self.pbp_data = nfl.load_pbp(years).to_pandas()
            print(f"✓ Loaded {len(self.pbp_data)} plays from {years}")
            
            # Load schedule data
            self.schedule_data = nfl.load_schedules(years).to_pandas()
            print(f"✓ Loaded {len(self.schedule_data)} games from schedule")
            
            # Load team descriptions (if available)
            try:
                self.team_info = nfl.load_teams().to_pandas()
                print(f"✓ Loaded team information for {len(self.team_info)} teams")
            except:
                self.team_info = None
                print("⚠ Team info not available")
            
        except Exception as e:
            print(f"Error loading data: {e}")
    
    def calculate_team_stats(self):
        """
        Calculate offensive and defensive scoring rates for each team
        """
        print("Calculating team statistics...")
        
        # Get all unique teams
        all_teams = set(self.pbp_data['posteam'].dropna()) | set(self.pbp_data['defteam'].dropna())
        all_teams.discard('nan')  # Remove any NaN values
        
        for team in all_teams:
            # Offensive stats (when team has possession)
            team_off = self.pbp_data[self.pbp_data['posteam'] == team]
            
            # Defensive stats (when team is defending)
            team_def = self.pbp_data[self.pbp_data['defteam'] == team]
            
            if len(team_off) > 0:
                # Calculate offensive scoring rate (points per drive)
                drives = team_off.groupby(['game_id', 'drive']).agg({
                    'posteam_score': 'max',
                    'defteam_score': 'max'
                }).reset_index()
                
                # Calculate points scored per drive
                drives['points_scored'] = drives.groupby('game_id')['posteam_score'].diff().fillna(drives['posteam_score'])
                drives = drives[drives['points_scored'] >= 0]  # Remove negative values
                
                if len(drives) > 0:
                    self.offensive_rates[team] = drives['points_scored'].mean()
                else:
                    self.offensive_rates[team] = 0.0
            else:
                self.offensive_rates[team] = 0.0
            
            if len(team_def) > 0:
                # Calculate defensive scoring rate allowed (points per drive allowed)
                drives_def = team_def.groupby(['game_id', 'drive']).agg({
                    'posteam_score': 'max',
                    'defteam_score': 'max'
                }).reset_index()
                
                # Calculate points allowed per drive
                drives_def['points_allowed'] = drives_def.groupby('game_id')['posteam_score'].diff().fillna(drives_def['posteam_score'])
                drives_def = drives_def[drives_def['points_allowed'] >= 0]  # Remove negative values
                
                if len(drives_def) > 0:
                    self.defensive_rates[team] = drives_def['points_allowed'].mean()
                else:
                    self.defensive_rates[team] = 0.0
            else:
                self.defensive_rates[team] = 0.0
            
            # Initialize ELO ratings
            self.team_ratings[team] = self.initial_rating
        
        print(f"✓ Calculated stats for {len(all_teams)} teams")
    
    def poisson_probability(self, lambda_home, lambda_away):
        """
        Calculate probability of home team winning using Poisson model
        
        Parameters:
        -----------
        lambda_home : float
            Expected points for home team
        lambda_away : float
            Expected points for away team
            
        Returns:
        --------
        tuple : (home_win_prob, away_win_prob, tie_prob)
        """
        home_win_prob = 0
        tie_prob = 0
        
        # Calculate probabilities for all possible score combinations
        for home_score in range(0, 50):  # Reasonable range for NFL scores (maybe to 75 to be safe)
            for away_score in range(0, 50): # poisson.pmg(k, mu) mu being rate, k being the x we're testing for
                prob = poisson.pmf(home_score, lambda_home) * poisson.pmf(away_score, lambda_away)
                
                if home_score > away_score: #TODO: this confuses me, why is home_win_prob += prob?
                    home_win_prob += prob
                elif home_score == away_score:
                    tie_prob += prob
        
        away_win_prob = 1 - home_win_prob - tie_prob
        
        return home_win_prob, away_win_prob, tie_prob
    
    def expected_points(self, team_rating, opp_rating, is_home=True):
        """
        Calculate expected points for a team based on ELO ratings
        
        Parameters:
        -----------
        team_rating : float
            Team's ELO rating
        opp_rating : float
            Opponent's ELO rating
        is_home : bool
            Whether team is playing at home
            
        Returns:
        --------
        tuple : (expected_points_for, expected_points_against)
        """
        # Calculate rating difference
        rating_diff = team_rating - opp_rating
        
        # Add home field advantage
        if is_home:
            rating_diff += self.home_advantage
        
        # Convert rating difference to expected points
        # Using a logistic function to map rating difference to points
        expected_point_diff = 7 * np.tanh(rating_diff / 200)
        
        # Base expected points (average NFL game score is around 23-24 points)
        base_points = 23.5
        
        # Calculate expected points for and against
        expected_for = base_points + expected_point_diff / 2
        expected_against = base_points - expected_point_diff / 2
        
        return max(0, expected_for), max(0, expected_against)
    
    def update_elo_ratings(self, home_team, away_team, home_score, away_score):
        """
        Update ELO ratings based on game result
        
        Parameters:
        -----------
        home_team : str
            Home team abbreviation
        away_team : str
            Away team abbreviation
        home_score : int
            Home team's final score
        away_score : int
            Away team's final score
        """
        # Get current ratings
        home_rating = self.team_ratings.get(home_team, self.initial_rating)
        away_rating = self.team_ratings.get(away_team, self.initial_rating)
        
        # Calculate expected points
        home_expected_for, home_expected_against = self.expected_points(
            home_rating, away_rating, is_home=True
        )
        away_expected_for, away_expected_against = self.expected_points(
            away_rating, home_rating, is_home=False
        )
        
        # Calculate actual performance vs expected
        home_performance = (home_score - away_score) - (home_expected_for - home_expected_against)
        away_performance = (away_score - home_score) - (away_expected_for - away_expected_against)
        
        # Update ratings
        home_rating_change = self.k_factor * home_performance / 7  # Scale by 7 points
        away_rating_change = self.k_factor * away_performance / 7
        
        self.team_ratings[home_team] = home_rating + home_rating_change
        self.team_ratings[away_team] = away_rating + away_rating_change
    
    def train_model(self, start_week=None, end_week=None, game_types=None):
        """
        Train the ELO model on historical data
        
        Parameters:
        -----------
        start_week : int, optional
            Starting week to train on (inclusive)
        end_week : int, optional
            Ending week to train on (inclusive)
        game_types : list, optional
            List of game types to include (e.g., ['REG'] for regular season only)
        """
        print("Training ELO model...")
        
        # Get completed games
        completed_games = self.schedule_data[
            (self.schedule_data['home_score'].notna()) & 
            (self.schedule_data['away_score'].notna())
        ].copy()
        
        # Filter by week range if specified
        if start_week is not None:
            completed_games = completed_games[completed_games['week'] >= start_week]
            print(f"Filtering to weeks {start_week}+")
        
        if end_week is not None:
            completed_games = completed_games[completed_games['week'] <= end_week]
            print(f"Filtering to weeks up to {end_week}")
        
        # Filter by game types if specified
        if game_types is not None:
            completed_games = completed_games[completed_games['game_type'].isin(game_types)]
            print(f"Filtering to game types: {game_types}")
        
        print(f"Processing {len(completed_games)} completed games...")
        
        # Show week distribution
        if len(completed_games) > 0:
            week_dist = completed_games['week'].value_counts().sort_index()
            print(f"Week distribution: {dict(week_dist)}")
        
        # Sort by date to process chronologically
        completed_games = completed_games.sort_values('gameday')
        
        for _, game in completed_games.iterrows():
            home_team = game['home_team']
            away_team = game['away_team']
            home_score = game['home_score']
            away_score = game['away_score']
            
            # Update ELO ratings
            self.update_elo_ratings(home_team, away_team, home_score, away_score)
        
        print("✓ ELO model training completed")
    
    def predict_game(self, home_team, away_team):
        """
        Predict game outcome between two teams
        
        Parameters:
        -----------
        home_team : str
            Home team abbreviation
        away_team : str
            Away team abbreviation
            
        Returns:
        --------
        dict : Prediction results
        """
        # Get current ratings
        home_rating = self.team_ratings.get(home_team, self.initial_rating)
        away_rating = self.team_ratings.get(away_team, self.initial_rating)
        
        # Calculate expected points
        home_expected_for, home_expected_against = self.expected_points(
            home_rating, away_rating, is_home=True
        )
        away_expected_for, away_expected_against = self.expected_points(
            away_rating, home_rating, is_home=False
        )
        
        # Use Poisson model for win probabilities
        home_win_prob, away_win_prob, tie_prob = self.poisson_probability(
            home_expected_for, away_expected_for
        )
        
        return {
            'home_team': home_team,
            'away_team': away_team,
            'home_rating': home_rating,
            'away_rating': away_rating,
            'home_expected_points': home_expected_for,
            'away_expected_points': away_expected_for,
            'home_win_probability': home_win_prob,
            'away_win_probability': away_win_prob,
            'tie_probability': tie_prob,
            'predicted_winner': home_team if home_win_prob > away_win_prob else away_team,
            'confidence': max(home_win_prob, away_win_prob)
        }
    
    def get_team_rankings(self):
        """
        Get current team rankings sorted by ELO rating
        
        Returns:
        --------
        pandas.DataFrame : Team rankings
        """
        rankings = []
        for team, rating in self.team_ratings.items():
            rankings.append({
                'team': team,
                'elo_rating': rating,
                'offensive_rate': self.offensive_rates.get(team, 0),
                'defensive_rate': self.defensive_rates.get(team, 0)
            })
        
        df = pd.DataFrame(rankings)
        df = df.sort_values('elo_rating', ascending=False)
        df['rank'] = range(1, len(df) + 1)
        
        return df
    
    def visualize_rankings(self, top_n=15):
        """
        Create visualizations of team rankings
        """
        rankings = self.get_team_rankings().head(top_n)
        
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 8))
        
        # ELO Ratings
        ax1.barh(range(len(rankings)), rankings['elo_rating'], color='steelblue')
        ax1.set_yticks(range(len(rankings)))
        ax1.set_yticklabels(rankings['team'])
        ax1.set_xlabel('ELO Rating')
        ax1.set_title(f'Top {top_n} NFL Teams by ELO Rating')
        ax1.invert_yaxis()
        
        # Add rating values on bars
        for i, v in enumerate(rankings['elo_rating']):
            ax1.text(v + 5, i, f'{v:.0f}', va='center')
        
        # Offensive vs Defensive Rates
        ax2.scatter(rankings['offensive_rate'], rankings['defensive_rate'], 
                   s=100, alpha=0.7, color='red')
        
        for i, team in enumerate(rankings['team']):
            ax2.annotate(team, 
                        (rankings.iloc[i]['offensive_rate'], 
                         rankings.iloc[i]['defensive_rate']),
                        xytext=(5, 5), textcoords='offset points', fontsize=8)
        
        ax2.set_xlabel('Offensive Rate (Points per Drive)')
        ax2.set_ylabel('Defensive Rate (Points Allowed per Drive)')
        ax2.set_title('Team Performance: Offense vs Defense')
        ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        plt.show()
    
    def model_accuracy(self):
        """
        Calculate model accuracy on historical games
        """
        print("Calculating model accuracy...")
        
        completed_games = self.schedule_data[
            (self.schedule_data['home_score'].notna()) & 
            (self.schedule_data['away_score'].notna())
        ].copy()
        
        correct_predictions = 0
        total_predictions = 0
        
        for _, game in completed_games.iterrows():
            home_team = game['home_team']
            away_team = game['away_team']
            home_score = game['home_score']
            away_score = game['away_score']
            
            # Get prediction
            prediction = self.predict_game(home_team, away_team)
            predicted_winner = prediction['predicted_winner']
            
            # Actual winner
            actual_winner = home_team if home_score > away_score else away_team
            
            if predicted_winner == actual_winner:
                correct_predictions += 1
            
            total_predictions += 1
        
        accuracy = correct_predictions / total_predictions if total_predictions > 0 else 0
        
        print(f"Model Accuracy: {accuracy:.3f} ({correct_predictions}/{total_predictions})")
        return accuracy

def main():
    """
    Main function to run the Poisson ELO model
    """
    print("🏈 NFL Poisson ELO Model")
    print("=" * 50)
    
    # Initialize model
    model = PoissonELOModel(initial_rating=1500, k_factor=32, home_advantage=25)
    
    # Load data
    model.load_nfl_data([2024])
    
    # Calculate team statistics
    model.calculate_team_stats()
    
    # Train model with different options
    print("\n" + "=" * 50)
    print("TRAINING OPTIONS")
    print("=" * 50)
    print("Choose training approach:")
    print("1. Full season (all weeks, all game types)")
    print("2. Regular season only (weeks 1-18)")
    print("3. Early season (weeks 1-8)")
    print("4. Mid season (weeks 5-12)")
    print("5. Late season (weeks 13-18)")
    print("6. Custom range")
    
    # For demonstration, let's use regular season only
    print("\nUsing Regular Season Only (weeks 1-18)...")
    model.train_model(start_week=1, end_week=5, game_types=['REG'])
    
    # Calculate accuracy
    accuracy = model.model_accuracy()
    
    # Show rankings
    print("\n" + "=" * 50)
    print("CURRENT TEAM RANKINGS")
    print("=" * 50)
    rankings = model.get_team_rankings()
    print(rankings.head(10).to_string(index=False))
    
    # Example predictions
    print("\n" + "=" * 50)
    print("EXAMPLE PREDICTIONS")
    print("=" * 50)
    
    # Get top teams for example predictions
    top_teams = rankings.head(6)['team'].tolist()
    
    print("Top teams:", top_teams)

    for i in range(0, len(top_teams)-1, 2):
        home_team = top_teams[i]
        away_team = top_teams[i+1]
        
        prediction = model.predict_game(home_team, away_team)
        
        print(f"\n{prediction['home_team']} (Home) vs {prediction['away_team']} (Away)")
        print(f"ELO Ratings: {prediction['home_rating']:.0f} vs {prediction['away_rating']:.0f}")
        print(f"Expected Score: {prediction['home_expected_points']:.1f} - {prediction['away_expected_points']:.1f}")
        print(f"Win Probabilities: {prediction['home_win_probability']:.1%} vs {prediction['away_win_probability']:.1%}")
        print(f"Predicted Winner: {prediction['predicted_winner']} ({prediction['confidence']:.1%} confidence)")
    
    # Create visualizations
    print("\nGenerating visualizations...")
    model.visualize_rankings()
    
    print("\n" + "=" * 50)
    print("Model training and analysis complete!")
    print("=" * 50)

if __name__ == "__main__":
    main()
