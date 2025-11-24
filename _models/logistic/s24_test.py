#!/usr/bin/env python3
"""
NFL Logistic Regression Model Testing - 2024 Season
===================================================

This script tests the logistic regression model on the 2024 NFL season:
- Trains on 2022-2023 seasons
- Tests on 2024 season weeks 6-18
- Uses rolling window approach (incorporates prior 2024 weeks as they become available)
- Measures week-by-week accuracy and overall performance

Author: NFL Bets Analysis
"""

import sys
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime
import nflreadpy as nfl
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score, log_loss, brier_score_loss
import warnings
warnings.filterwarnings('ignore')

class LogisticRegressionTester:
    """
    Class to test logistic regression model on 2024 NFL season
    """
    
    def __init__(self, train_seasons=[2022, 2023], test_season=2024):
        """
        Initialize the tester
        
        Parameters:
        -----------
        train_seasons : list
            Seasons to train on
        test_season : int
            Season to test on
        """
        self.train_seasons = train_seasons
        self.test_season = test_season
        self.pbp_data = None
        self.games_data = None
        self.cumulative_stats = {}
        self.model = LogisticRegression(random_state=42, max_iter=1000)
        self.scaler = StandardScaler()
        self.feature_columns = []
        self.results = []
        
        print(f"Initialized Logistic Regression Tester")
        print(f"Training seasons: {train_seasons}")
        print(f"Test season: {test_season}")
    
    def load_data(self):
        """Load NFL data for all seasons"""
        print("\n" + "="*60)
        print("LOADING NFL DATA")
        print("="*60)
        
        all_seasons = self.train_seasons + [self.test_season]
        
        print(f"Loading data for seasons: {all_seasons}")
        
        # Load play-by-play data
        print("Loading play-by-play data...")
        self.pbp_data = nfl.load_pbp(all_seasons).to_pandas()
        print(f"✓ Loaded {len(self.pbp_data):,} plays")
        
        # Load schedule data
        print("Loading schedule data...")
        self.games_data = nfl.load_schedules(all_seasons).to_pandas()
        print(f"✓ Loaded {len(self.games_data):,} games")
        
        # Filter to regular season only
        self.games_data = self.games_data[
            self.games_data['game_type'] == 'REG'
        ].copy()
        
        print(f"✓ Filtered to {len(self.games_data):,} regular season games")
    
    def calculate_team_stats_efficient(self):
        """Calculate team statistics efficiently for all weeks"""
        print("\n" + "="*60)
        print("CALCULATING TEAM STATISTICS")
        print("="*60)
        
        # Create team-game level data
        print("[1/3] Processing game-level statistics...")
        games = self.games_data.copy()
        
        # Home perspective
        home_games = games[['game_id', 'season', 'week', 'home_team', 'away_team', 
                           'home_score', 'away_score']].copy()
        home_games.columns = ['game_id', 'season', 'week', 'team', 'opponent', 
                             'points_for', 'points_against']
        home_games['won'] = (home_games['points_for'] > home_games['points_against']).astype(int)
        
        # Away perspective
        away_games = games[['game_id', 'season', 'week', 'away_team', 'home_team', 
                           'away_score', 'home_score']].copy()
        away_games.columns = ['game_id', 'season', 'week', 'team', 'opponent', 
                             'points_for', 'points_against']
        away_games['won'] = (away_games['points_for'] > away_games['points_against']).astype(int)
        
        all_team_games = pd.concat([home_games, away_games], ignore_index=True)
        all_team_games = all_team_games.sort_values(['season', 'week', 'team'])
        
        print(f"✓ Processed {len(all_team_games):,} team-game records")
        
        # Calculate efficiency metrics from play-by-play
        print("[2/3] Calculating efficiency metrics...")
        pbp = self.pbp_data[self.pbp_data['play_type'].isin(['pass', 'run'])].copy()
        
        # Offensive stats
        off_stats = pbp.groupby(['season', 'week', 'posteam']).apply(
            lambda x: pd.Series({
                'pass_attempts': len(x[x['play_type'] == 'pass']),
                'completions': x[x['play_type'] == 'pass']['complete_pass'].sum(),
                'pass_yards': x[x['play_type'] == 'pass']['passing_yards'].sum(),
                'interceptions': x['interception'].sum(),
                'rush_attempts': len(x[x['play_type'] == 'run']),
                'rush_yards': x[x['play_type'] == 'run']['rushing_yards'].sum(),
                'fumbles_lost': x['fumble_lost'].sum(),
            })
        ).reset_index()
        off_stats.columns = ['season', 'week', 'team', 'pass_attempts', 'completions', 
                            'pass_yards', 'interceptions', 'rush_attempts', 'rush_yards', 'fumbles_lost']
        
        # Defensive stats
        def_stats = pbp.groupby(['season', 'week', 'defteam']).apply(
            lambda x: pd.Series({
                'def_pass_attempts': len(x[x['play_type'] == 'pass']),
                'def_completions': x[x['play_type'] == 'pass']['complete_pass'].sum(),
                'def_pass_yards': x[x['play_type'] == 'pass']['passing_yards'].sum(),
                'def_rush_attempts': len(x[x['play_type'] == 'run']),
                'def_rush_yards': x[x['play_type'] == 'run']['rushing_yards'].sum(),
            })
        ).reset_index()
        def_stats.columns = ['season', 'week', 'team', 'def_pass_attempts', 'def_completions',
                            'def_pass_yards', 'def_rush_attempts', 'def_rush_yards']
        
        # Merge all stats
        full_df = all_team_games.merge(off_stats, on=['season', 'week', 'team'], how='left')
        full_df = full_df.merge(def_stats, on=['season', 'week', 'team'], how='left')
        full_df = full_df.fillna(0)
        
        print(f"✓ Calculated efficiency metrics")
        
        # Build cumulative stats
        print("[3/3] Building cumulative statistics...")
        full_df = full_df.sort_values(['team', 'season', 'week'])
        
        teams = full_df['team'].unique()
        
        for team in teams:
            team_data = full_df[full_df['team'] == team].copy()
            
            for idx, row in team_data.iterrows():
                season = row['season']
                week = row['week']
                
                # Get all data up to (but not including) this week
                if season in self.train_seasons:
                    prior_data = team_data[
                        ((team_data['season'].isin(self.train_seasons)) & 
                         (team_data['season'] < season)) |
                        ((team_data['season'] == season) & (team_data['week'] < week))
                    ]
                else:
                    prior_data = team_data[
                        (team_data['season'].isin(self.train_seasons)) |
                        ((team_data['season'] == season) & (team_data['week'] < week))
                    ]
                
                if len(prior_data) == 0:
                    continue
                
                games_played = len(prior_data)
                
                stats = {
                    'team': team,
                    'games_played': games_played,
                    'win_pct': prior_data['won'].mean(),
                    'ppg': prior_data['points_for'].mean(),
                    'pa_pg': prior_data['points_against'].mean(),
                    'point_diff': prior_data['points_for'].mean() - prior_data['points_against'].mean(),
                    'completion_pct': (prior_data['completions'].sum() / prior_data['pass_attempts'].sum() 
                                      if prior_data['pass_attempts'].sum() > 0 else 0),
                    'yards_per_pass': (prior_data['pass_yards'].sum() / prior_data['pass_attempts'].sum() 
                                      if prior_data['pass_attempts'].sum() > 0 else 0),
                    'yards_per_rush': (prior_data['rush_yards'].sum() / prior_data['rush_attempts'].sum() 
                                      if prior_data['rush_attempts'].sum() > 0 else 0),
                    'int_rate': (prior_data['interceptions'].sum() / prior_data['pass_attempts'].sum() 
                                if prior_data['pass_attempts'].sum() > 0 else 0),
                    'turnover_pg': ((prior_data['interceptions'].sum() + prior_data['fumbles_lost'].sum()) / 
                                   games_played),
                    'def_completion_pct': (prior_data['def_completions'].sum() / prior_data['def_pass_attempts'].sum() 
                                          if prior_data['def_pass_attempts'].sum() > 0 else 0),
                    'def_yards_per_pass': (prior_data['def_pass_yards'].sum() / prior_data['def_pass_attempts'].sum() 
                                          if prior_data['def_pass_attempts'].sum() > 0 else 0),
                    'def_yards_per_rush': (prior_data['def_rush_yards'].sum() / prior_data['def_rush_attempts'].sum() 
                                          if prior_data['def_rush_attempts'].sum() > 0 else 0),
                }
                
                self.cumulative_stats[(season, week, team)] = stats
        
        print(f"✓ Built cumulative stats for {len(self.cumulative_stats):,} team-week combinations")
    
    def create_game_features(self, home_stats: dict, away_stats: dict) -> dict:
        """Create differential features between home and away teams"""
        skip_cols = ['team', 'games_played']
        
        features = {}
        for key in home_stats.keys():
            if key in skip_cols:
                continue
            features[f'{key}_diff'] = home_stats[key] - away_stats[key]
        
        return features
    
    def prepare_dataset(self, seasons: list, weeks: list = None) -> pd.DataFrame:
        """
        Prepare dataset for given seasons and weeks
        
        Parameters:
        -----------
        seasons : list
            Seasons to include
        weeks : list, optional
            Specific weeks to include (None = all weeks)
        """
        games = self.games_data[self.games_data['season'].isin(seasons)].copy()
        
        if weeks is not None:
            games = games[games['week'].isin(weeks)]
        
        dataset = []
        
        for _, game in games.iterrows():
            season = game['season']
            week = game['week']
            home_team = game['home_team']
            away_team = game['away_team']
            
            if pd.isna(home_team) or pd.isna(away_team):
                continue
            
            home_key = (season, week, home_team)
            away_key = (season, week, away_team)
            
            if home_key not in self.cumulative_stats or away_key not in self.cumulative_stats:
                continue
            
            home_stats = self.cumulative_stats[home_key]
            away_stats = self.cumulative_stats[away_key]
            
            features = self.create_game_features(home_stats, away_stats)
            
            features['target'] = 1 if game['home_score'] > game['away_score'] else 0
            features['game_id'] = game['game_id']
            features['season'] = season
            features['week'] = week
            features['home_team'] = home_team
            features['away_team'] = away_team
            features['home_score'] = game['home_score']
            features['away_score'] = game['away_score']
            
            dataset.append(features)
        
        return pd.DataFrame(dataset)
    
    def train_model(self):
        """Train the model on 2022-2023 data"""
        print("\n" + "="*60)
        print("TRAINING MODEL ON 2022-2023 DATA")
        print("="*60)
        
        train_df = self.prepare_dataset(self.train_seasons)
        
        print(f"\nTraining games: {len(train_df)}")
        print(f"Home wins: {train_df['target'].sum()} ({train_df['target'].mean()*100:.1f}%)")
        
        metadata_cols = ['target', 'game_id', 'season', 'week', 'home_team', 
                        'away_team', 'home_score', 'away_score']
        self.feature_columns = [col for col in train_df.columns if col not in metadata_cols]
        
        X_train = train_df[self.feature_columns].fillna(0)
        y_train = train_df['target']
        
        print(f"Features: {len(self.feature_columns)}")
        
        # Train model
        X_train_scaled = self.scaler.fit_transform(X_train)
        self.model.fit(X_train_scaled, y_train)
        
        # Training accuracy
        train_pred = self.model.predict(X_train_scaled)
        train_acc = accuracy_score(y_train, train_pred)
        
        print(f"\n✓ Model trained successfully")
        print(f"Training accuracy: {train_acc:.3f}")
    
    def test_week(self, test_week: int):
        """
        Test model on a specific week of 2024
        
        Parameters:
        -----------
        test_week : int
            Week number to test
        """
        print(f"\n{'='*60}")
        print(f"TESTING WEEK {test_week}")
        print(f"{'='*60}")
        
        # Get games for this week
        test_df = self.prepare_dataset([self.test_season], weeks=[test_week])
        
        if len(test_df) == 0:
            print(f"No games found for week {test_week}")
            return None
        
        print(f"Testing on {len(test_df)} games")
        
        X_test = test_df[self.feature_columns].fillna(0)
        y_test = test_df['target']
        
        X_test_scaled = self.scaler.transform(X_test)
        
        # Predictions
        y_pred = self.model.predict(X_test_scaled)
        y_pred_proba = self.model.predict_proba(X_test_scaled)[:, 1]
        
        # Metrics
        accuracy = accuracy_score(y_test, y_pred)
        logloss = log_loss(y_test, y_pred_proba)
        brier = brier_score_loss(y_test, y_pred_proba)
        
        # Detailed predictions
        predictions = []
        correct_count = 0
        
        for idx, row in test_df.iterrows():
            pred_idx = test_df.index.get_loc(idx)
            predicted_home_win = y_pred[pred_idx]
            home_win_prob = y_pred_proba[pred_idx]
            
            actual_winner = row['home_team'] if row['target'] == 1 else row['away_team']
            predicted_winner = row['home_team'] if predicted_home_win == 1 else row['away_team']
            is_correct = predicted_winner == actual_winner
            
            if is_correct:
                correct_count += 1
            
            predictions.append({
                'home_team': row['home_team'],
                'away_team': row['away_team'],
                'home_score': row['home_score'],
                'away_score': row['away_score'],
                'predicted_winner': predicted_winner,
                'actual_winner': actual_winner,
                'home_win_prob': home_win_prob,
                'away_win_prob': 1 - home_win_prob,
                'correct': is_correct
            })
        
        result = {
            'week': test_week,
            'num_games': len(test_df),
            'correct': correct_count,
            'accuracy': accuracy,
            'log_loss': logloss,
            'brier_score': brier,
            'predictions': predictions
        }
        
        print(f"✓ Accuracy: {accuracy:.3f} ({correct_count}/{len(test_df)})")
        print(f"  Log Loss: {logloss:.4f}")
        print(f"  Brier Score: {brier:.4f}")
        
        return result
    
    def run_test_weeks(self, start_week=6, end_week=18):
        """
        Test model on weeks 6-18 of 2024 season
        
        Parameters:
        -----------
        start_week : int
            Starting week
        end_week : int
            Ending week
        """
        print("\n" + "="*60)
        print(f"TESTING 2024 SEASON: WEEKS {start_week}-{end_week}")
        print("="*60)
        
        results = []
        
        for week in range(start_week, end_week + 1):
            result = self.test_week(week)
            if result:
                results.append(result)
        
        self.results = results
        return results
    
    def analyze_results(self):
        """Analyze and display results"""
        if not self.results:
            print("No results to analyze!")
            return
        
        print("\n" + "="*60)
        print("OVERALL RESULTS SUMMARY")
        print("="*60)
        
        # Create results DataFrame
        results_df = pd.DataFrame([
            {
                'week': r['week'],
                'games': r['num_games'],
                'correct': r['correct'],
                'accuracy': r['accuracy'],
                'log_loss': r['log_loss'],
                'brier_score': r['brier_score']
            }
            for r in self.results
        ])
        
        print("\n" + results_df.to_string(index=False))
        
        # Overall statistics
        total_games = results_df['games'].sum()
        total_correct = results_df['correct'].sum()
        overall_accuracy = total_correct / total_games
        
        print(f"\n{'='*60}")
        print(f"📊 SUMMARY STATISTICS")
        print(f"{'='*60}")
        print(f"Total Games: {total_games}")
        print(f"Total Correct: {total_correct}")
        print(f"Overall Accuracy: {overall_accuracy:.3f} ({overall_accuracy*100:.1f}%)")
        print(f"Average Weekly Accuracy: {results_df['accuracy'].mean():.3f}")
        print(f"Best Week: Week {results_df.loc[results_df['accuracy'].idxmax(), 'week']} ({results_df['accuracy'].max():.3f})")
        print(f"Worst Week: Week {results_df.loc[results_df['accuracy'].idxmin(), 'week']} ({results_df['accuracy'].min():.3f})")
        print(f"Std Dev: {results_df['accuracy'].std():.3f}")
        print(f"Average Log Loss: {results_df['log_loss'].mean():.4f}")
        print(f"Average Brier Score: {results_df['brier_score'].mean():.4f}")
        
        return results_df
    
    def visualize_results(self, save_path=None):
        """Create visualizations of the results"""
        if not self.results:
            print("No results to visualize!")
            return
        
        results_df = pd.DataFrame([
            {
                'week': r['week'],
                'accuracy': r['accuracy'],
                'games': r['num_games']
            }
            for r in self.results
        ])
        
        fig, ax = plt.subplots(figsize=(14, 6))
        
        # Bar plot of accuracy by week
        bars = ax.bar(results_df['week'], results_df['accuracy'], 
                     alpha=0.7, edgecolor='black', linewidth=1.5)
        
        # Color bars based on performance
        for i, (_, row) in enumerate(results_df.iterrows()):
            if row['accuracy'] >= 0.65:
                bars[i].set_color('green')
            elif row['accuracy'] >= 0.55:
                bars[i].set_color('orange')
            else:
                bars[i].set_color('red')
        
        # Add horizontal lines
        ax.axhline(0.5, color='red', linestyle='--', linewidth=2, 
                  label='Random Baseline (50%)', alpha=0.7)
        ax.axhline(results_df['accuracy'].mean(), color='blue', 
                  linestyle='--', linewidth=2, 
                  label=f'Average ({results_df["accuracy"].mean():.1%})', alpha=0.7)
        
        # Add game counts on bars
        for _, row in results_df.iterrows():
            ax.text(row['week'], row['accuracy'] + 0.02, 
                   f"{row['accuracy']:.1%}\n({row['games']} games)", 
                   ha='center', fontsize=9)
        
        ax.set_xlabel('Week', fontsize=12)
        ax.set_ylabel('Accuracy', fontsize=12)
        ax.set_title('2024 Season: Logistic Regression Model Performance by Week', 
                    fontsize=14, fontweight='bold')
        ax.set_ylim([0, 1])
        ax.set_xticks(results_df['week'])
        ax.legend(fontsize=11)
        ax.grid(True, alpha=0.3, axis='y')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            print(f"\n✓ Visualization saved to: {save_path}")
            plt.close()
        else:
            plt.show()
    
    def show_detailed_predictions(self, week=None):
        """Show detailed predictions for specific week(s)"""
        if not self.results:
            print("No results to show!")
            return
        
        if week:
            results_to_show = [r for r in self.results if r['week'] == week]
            if not results_to_show:
                print(f"No results found for week {week}")
                return
        else:
            results_to_show = self.results
        
        for result in results_to_show:
            print(f"\n{'='*60}")
            print(f"WEEK {result['week']} - DETAILED PREDICTIONS")
            print(f"Accuracy: {result['accuracy']:.1%} ({result['correct']}/{result['num_games']})")
            print(f"{'='*60}")
            
            for pred in result['predictions']:
                status = "✓" if pred['correct'] else "✗"
                print(f"\n{status} {pred['away_team']} @ {pred['home_team']}")
                print(f"   Predicted: {pred['predicted_winner']} (Home: {pred['home_win_prob']:.1%}, Away: {pred['away_win_prob']:.1%})")
                print(f"   Actual: {pred['actual_winner']} ({pred['away_team']} {pred['away_score']}, {pred['home_team']} {pred['home_score']})")
    
    def save_results_to_csv(self, output_dir='results'):
        """Save results to CSV files"""
        import os
        os.makedirs(output_dir, exist_ok=True)
        
        # Summary results
        summary_df = pd.DataFrame([
            {
                'week': r['week'],
                'games': r['num_games'],
                'correct': r['correct'],
                'accuracy': r['accuracy'],
                'log_loss': r['log_loss'],
                'brier_score': r['brier_score']
            }
            for r in self.results
        ])
        summary_df.to_csv(f'{output_dir}/logistic_2024_weekly_results.csv', index=False)
        print(f"✓ Saved weekly results to: {output_dir}/logistic_2024_weekly_results.csv")
        
        # Detailed game predictions
        all_predictions = []
        for result in self.results:
            for pred in result['predictions']:
                all_predictions.append({
                    'week': result['week'],
                    'home_team': pred['home_team'],
                    'away_team': pred['away_team'],
                    'home_score': pred['home_score'],
                    'away_score': pred['away_score'],
                    'predicted_winner': pred['predicted_winner'],
                    'actual_winner': pred['actual_winner'],
                    'home_win_prob': pred['home_win_prob'],
                    'away_win_prob': pred['away_win_prob'],
                    'correct': pred['correct']
                })
        
        predictions_df = pd.DataFrame(all_predictions)
        predictions_df.to_csv(f'{output_dir}/logistic_2024_game_results.csv', index=False)
        print(f"✓ Saved game results to: {output_dir}/logistic_2024_game_results.csv")


def main():
    """Main function to run the test"""
    print("🏈 NFL Logistic Regression Model - 2024 Season Test")
    print("="*60)
    
    # Initialize tester
    tester = LogisticRegressionTester(train_seasons=[2022, 2023], test_season=2024)
    
    # Load data
    tester.load_data()
    
    # Calculate team statistics
    tester.calculate_team_stats_efficient()
    
    # Train model on 2022-2023
    tester.train_model()
    
    # Test on weeks 6-18 of 2024
    results = tester.run_test_weeks(start_week=6, end_week=18)
    
    # Analyze results
    results_df = tester.analyze_results()
    
    # Show detailed predictions for a few sample weeks
    print(f"\n{'='*60}")
    print("SAMPLE DETAILED PREDICTIONS")
    print(f"{'='*60}")
    for week in [6, 10, 14]:
        if any(r['week'] == week for r in results):
            tester.show_detailed_predictions(week)
    
    # Create visualizations
    print("\nGenerating visualizations...")
    tester.visualize_results(save_path='results/logistic_weekly_performance.png')
    
    # Save results to CSV
    print("\nSaving results to CSV...")
    tester.save_results_to_csv(output_dir='results')
    
    print(f"\n{'='*60}")
    print("✓ Testing complete!")
    print(f"{'='*60}")


if __name__ == "__main__":
    main()

