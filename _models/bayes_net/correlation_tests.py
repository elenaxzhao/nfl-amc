"""
Comprehensive Correlation Analysis for NFL Data Variables vs Win/Loss Outcomes
"""

import nflreadpy as nfl
import pandas as pd
import numpy as np
from scipy.stats import pearsonr, spearmanr
import matplotlib.pyplot as plt
import seaborn as sns
from typing import Dict, List, Tuple
import warnings
warnings.filterwarnings('ignore')

# Try to import interactive plotting libraries
try:
    import plotly.graph_objects as go
    import plotly.express as px
    from plotly.subplots import make_subplots
    import plotly.offline as pyo
    PLOTLY_AVAILABLE = True
except ImportError:
    print("Plotly not available, using matplotlib for static plots")
    PLOTLY_AVAILABLE = False

class NFLCorrelationAnalyzer:
    def __init__(self, seasons: List[int] = None):
        """
        Initialize the correlation analyzer
        
        Args:
            seasons: List of seasons to analyze (default: last 3 seasons)
        """
        if seasons is None:
            current_season = nfl.get_current_season()
            self.seasons = [current_season - 2, current_season - 1, current_season]
        else:
            self.seasons = seasons
            
        self.correlation_results = {}
        self.data_sources = {}
        
    def load_all_data(self):
        """Load all available NFL data sources"""
        print("Loading NFL data from all sources...")
        
        # Always load schedules first
        print("Loading schedules and results...")
        self.data_sources['schedules'] = nfl.load_schedules(self.seasons)
        
        # Load other data sources with error handling
        data_sources_to_try = [
            ('pbp', 'play-by-play data', lambda: nfl.load_pbp(self.seasons)),
            ('team_stats', 'team stats', lambda: nfl.load_team_stats(self.seasons)),
            ('player_stats', 'player stats', lambda: nfl.load_player_stats(self.seasons)),
            ('injuries', 'injury data', lambda: nfl.load_injuries(self.seasons)),
            ('snap_counts', 'snap counts', lambda: nfl.load_snap_counts(self.seasons)),
            ('depth_charts', 'depth charts', lambda: nfl.load_depth_charts(self.seasons)),
            ('rosters', 'rosters', lambda: nfl.load_rosters(self.seasons))
        ]
        
        for source_name, description, load_func in data_sources_to_try:
            try:
                print(f"Loading {description}...")
                self.data_sources[source_name] = load_func()
                print(f"✓ {description} loaded successfully")
            except Exception as e:
                print(f"✗ Failed to load {description}: {e}")
                # Don't add to data_sources if it fails
        
        print(f"Data loading complete! Loaded {len(self.data_sources)} data sources")
        print(f"Available sources: {list(self.data_sources.keys())}")
    
    def create_game_level_dataset(self):
        """Create a comprehensive game-level dataset with all available features"""
        print("Creating comprehensive game-level dataset...")
        
        # Start with schedules as base
        schedules = self.data_sources['schedules'].to_pandas()
        
        # Check available columns
        print(f"Available columns in schedules: {list(schedules.columns)}")
        
        # Filter to regular season games only (check different possible column names)
        season_type_col = None
        for col in ['season_type', 'game_type', 'type']:
            if col in schedules.columns:
                season_type_col = col
                break
        
        if season_type_col:
            if 'REG' in schedules[season_type_col].unique():
                schedules = schedules[schedules[season_type_col] == 'REG'].copy()
            elif 'Regular Season' in schedules[season_type_col].unique():
                schedules = schedules[schedules[season_type_col] == 'Regular Season'].copy()
            else:
                print(f"Available season types: {schedules[season_type_col].unique()}")
        else:
            print("No season type column found, using all games")
        
        # Create win/loss outcome for home team
        schedules['home_win'] = (schedules['home_score'] > schedules['away_score']).astype(int)
        schedules['away_win'] = (schedules['away_score'] > schedules['home_score']).astype(int)
        
        # Extract game features
        game_features = []
        
        for _, game in schedules.iterrows():
            try:
                game_id = game['game_id']
                season = game['season']
                week = game['week']
                home_team = game['home_team']
                away_team = game['away_team']
                
                feature_row = {
                    'game_id': game_id,
                    'season': season,
                    'week': week,
                    'home_team': home_team,
                    'away_team': away_team,
                    'home_win': game['home_win'],
                    'away_win': game['away_win'],
                    'home_score': game['home_score'],
                    'away_score': game['away_score'],
                    'total_score': game['home_score'] + game['away_score'],
                    'score_diff': game['home_score'] - game['away_score'],
                    'divisional_game': game.get('divisional_game', 0),
                    'conference_game': game.get('conference_game', 0),
                    'home_rest_days': game.get('home_rest_days', 7),
                    'away_rest_days': game.get('away_rest_days', 7),
                    'weather': game.get('weather', ''),
                    'temp': game.get('temp', np.nan),
                    'wind': game.get('wind', np.nan),
                    'surface': game.get('surface', ''),
                    'roof': game.get('roof', ''),
                    'stadium_id': game.get('stadium_id', ''),
                }
            except Exception as e:
                print(f"Error processing game {game.get('game_id', 'unknown')}: {e}")
                continue
            
            # Add team stats features if available
            if 'team_stats' in self.data_sources:
                team_features = self._extract_team_features(game_id, home_team, away_team, season)
                feature_row.update(team_features)
            
            # Add player stats features if available
            if 'player_stats' in self.data_sources:
                player_features = self._extract_player_features(game_id, home_team, away_team, season)
                feature_row.update(player_features)
            
            # Add injury features if available
            if 'injuries' in self.data_sources:
                injury_features = self._extract_injury_features(game_id, home_team, away_team, season, week)
                feature_row.update(injury_features)
            
            game_features.append(feature_row)
        
        self.game_dataset = pd.DataFrame(game_features)
        print(f"Created dataset with {len(self.game_dataset)} games and {len(self.game_dataset.columns)} features")
        
        return self.game_dataset
    
    def _extract_team_features(self, game_id: str, home_team: str, away_team: str, season: int) -> Dict:
        """Extract team-level features for a game"""
        features = {}
        
        try:
            if 'team_stats' not in self.data_sources:
                return features
                
            team_stats = self.data_sources['team_stats'].to_pandas()
            
            # Check if game_id column exists
            if 'game_id' not in team_stats.columns:
                print(f"game_id column not found in team_stats. Available columns: {list(team_stats.columns)}")
                return features
            
            game_team_stats = team_stats[team_stats['game_id'] == game_id]
            
            if len(game_team_stats) > 0:
                # Check if team column exists
                team_col = None
                for col in ['team', 'recent_team', 'team_abbr']:
                    if col in game_team_stats.columns:
                        team_col = col
                        break
                
                if team_col:
                    home_stats = game_team_stats[game_team_stats[team_col] == home_team]
                    away_stats = game_team_stats[game_team_stats[team_col] == away_team]
                    
                    # Common team stats columns to look for
                    stat_cols = ['passing_yards', 'rushing_yards', 'total_yards', 'turnovers', 
                               'penalties', 'penalty_yards', 'passing_tds', 'rushing_tds']
                    
                    if len(home_stats) > 0:
                        for col in stat_cols:
                            if col in home_stats.columns:
                                features[f'home_{col}'] = home_stats.iloc[0][col]
                    
                    if len(away_stats) > 0:
                        for col in stat_cols:
                            if col in away_stats.columns:
                                features[f'away_{col}'] = away_stats.iloc[0][col]
        
        except Exception as e:
            print(f"Error extracting team features for game {game_id}: {e}")
        
        return features
    
    def _extract_player_features(self, game_id: str, home_team: str, away_team: str, season: int) -> Dict:
        """Extract player-level features for a game"""
        features = {}
        
        try:
            if 'player_stats' not in self.data_sources:
                return features
                
            player_stats = self.data_sources['player_stats'].to_pandas()
            
            # Check if game_id column exists
            if 'game_id' not in player_stats.columns:
                print(f"game_id column not found in player_stats. Available columns: {list(player_stats.columns)}")
                return features
            
            game_player_stats = player_stats[player_stats['game_id'] == game_id]
            
            if len(game_player_stats) > 0:
                # Check if team column exists
                team_col = None
                for col in ['recent_team', 'team', 'team_abbr']:
                    if col in game_player_stats.columns:
                        team_col = col
                        break
                
                if team_col:
                    home_players = game_player_stats[game_player_stats[team_col] == home_team]
                    away_players = game_player_stats[game_player_stats[team_col] == away_team]
                    
                    # Aggregate key player stats
                    for team, prefix in [(home_players, 'home'), (away_players, 'away')]:
                        if len(team) > 0:
                            # QB rating
                            qb_stats = team[team['position'] == 'QB']
                            if len(qb_stats) > 0 and 'passing_rating' in qb_stats.columns:
                                features[f'{prefix}_qb_rating'] = qb_stats['passing_rating'].mean()
                            else:
                                features[f'{prefix}_qb_rating'] = np.nan
                            
                            # Total rushing yards
                            if 'rushing_yards' in team.columns:
                                features[f'{prefix}_total_rushing'] = team['rushing_yards'].sum()
                            else:
                                features[f'{prefix}_total_rushing'] = np.nan
                            
                            # Total receiving yards
                            if 'receiving_yards' in team.columns:
                                features[f'{prefix}_total_receiving'] = team['receiving_yards'].sum()
                            else:
                                features[f'{prefix}_total_receiving'] = np.nan
                            
                            # Total touchdowns
                            td_cols = ['rushing_tds', 'receiving_tds']
                            if all(col in team.columns for col in td_cols):
                                features[f'{prefix}_total_touchdowns'] = team['rushing_tds'].sum() + team['receiving_tds'].sum()
                            else:
                                features[f'{prefix}_total_touchdowns'] = np.nan
        
        except Exception as e:
            print(f"Error extracting player features for game {game_id}: {e}")
        
        return features
    
    def _extract_injury_features(self, game_id: str, home_team: str, away_team: str, season: int, week: int) -> Dict:
        """Extract injury-related features for a game"""
        features = {}
        
        try:
            if 'injuries' not in self.data_sources:
                return features
                
            injuries = self.data_sources['injuries'].to_pandas()
            
            # Check required columns
            required_cols = ['season', 'week', 'team']
            if not all(col in injuries.columns for col in required_cols):
                print(f"Required columns not found in injuries data. Available: {list(injuries.columns)}")
                return features
            
            # Filter injuries for the specific week
            week_injuries = injuries[(injuries['season'] == season) & (injuries['week'] == week)]
            
            if len(week_injuries) > 0:
                home_injuries = week_injuries[week_injuries['team'] == home_team]
                away_injuries = week_injuries[week_injuries['team'] == away_team]
                
                features['home_injured_players'] = len(home_injuries)
                features['away_injured_players'] = len(away_injuries)
                
                # Key position injuries (if position column exists)
                if 'position' in week_injuries.columns:
                    key_positions = ['QB', 'RB', 'WR', 'TE']
                    features['home_key_injuries'] = len(home_injuries[home_injuries['position'].isin(key_positions)])
                    features['away_key_injuries'] = len(away_injuries[away_injuries['position'].isin(key_positions)])
                else:
                    features['home_key_injuries'] = 0
                    features['away_key_injuries'] = 0
        
        except Exception as e:
            print(f"Error extracting injury features for game {game_id}: {e}")
        
        return features
    
    def calculate_correlations(self):
        """Calculate correlations between all features and win/loss outcomes"""
        print("Calculating correlations with win/loss outcomes...")
        
        # Prepare data for correlation analysis
        df = self.game_dataset.copy()
        
        # Remove non-numeric columns and ID columns
        exclude_cols = ['game_id', 'home_team', 'away_team', 'weather', 'surface', 'roof', 'stadium_id']
        numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
        numeric_cols = [col for col in numeric_cols if col not in exclude_cols]
        
        correlation_results = []
        
        # Calculate correlations with home_win
        for col in numeric_cols:
            if col != 'home_win' and col != 'away_win':
                try:
                    # Remove NaN values
                    valid_data = df[[col, 'home_win']].dropna()
                    
                    if len(valid_data) > 10:  # Need sufficient data points
                        # Pearson correlation
                        pearson_corr, pearson_p = pearsonr(valid_data[col], valid_data['home_win'])
                        
                        # Spearman correlation (rank-based, more robust)
                        spearman_corr, spearman_p = spearmanr(valid_data[col], valid_data['home_win'])
                        
                        correlation_results.append({
                            'variable': col,
                            'pearson_correlation': pearson_corr,
                            'pearson_p_value': pearson_p,
                            'spearman_correlation': spearman_corr,
                            'spearman_p_value': spearman_p,
                            'sample_size': len(valid_data),
                            'mean_value': valid_data[col].mean(),
                            'std_value': valid_data[col].std()
                        })
                
                except Exception as e:
                    print(f"Error calculating correlation for {col}: {e}")
        
        self.correlation_results = pd.DataFrame(correlation_results)
        
        # Sort by absolute correlation strength
        self.correlation_results['abs_pearson'] = abs(self.correlation_results['pearson_correlation'])
        self.correlation_results['abs_spearman'] = abs(self.correlation_results['spearman_correlation'])
        
        # Sort by strongest correlations
        self.correlation_results = self.correlation_results.sort_values('abs_spearman', ascending=False)
        
        print(f"Calculated correlations for {len(self.correlation_results)} variables")
        
        return self.correlation_results
    
    def generate_report(self):
        """Generate a comprehensive correlation report"""
        print("\n" + "="*80)
        print("NFL DATA CORRELATION ANALYSIS REPORT")
        print("="*80)
        
        if self.correlation_results.empty:
            print("No correlation results available. Run calculate_correlations() first.")
            return
        
        # Top correlations
        print("\nTOP 20 STRONGEST CORRELATIONS WITH HOME WIN:")
        print("-" * 60)
        top_correlations = self.correlation_results.head(20)
        
        for _, row in top_correlations.iterrows():
            significance = "***" if row['spearman_p_value'] < 0.001 else "**" if row['spearman_p_value'] < 0.01 else "*" if row['spearman_p_value'] < 0.05 else ""
            print(f"{row['variable']:<30} | Spearman: {row['spearman_correlation']:6.3f} {significance:<3} | Pearson: {row['pearson_correlation']:6.3f} | n={row['sample_size']}")
        
        # Statistical significance summary
        print(f"\nSTATISTICAL SIGNIFICANCE SUMMARY:")
        print(f"Variables with p < 0.001: {len(self.correlation_results[self.correlation_results['spearman_p_value'] < 0.001])}")
        print(f"Variables with p < 0.01:  {len(self.correlation_results[self.correlation_results['spearman_p_value'] < 0.01])}")
        print(f"Variables with p < 0.05:  {len(self.correlation_results[self.correlation_results['spearman_p_value'] < 0.05])}")
        print(f"Total variables analyzed: {len(self.correlation_results)}")
        
        # Variable categories
        print(f"\nVARIABLE CATEGORIES:")
        categories = {
            'Score-related': [col for col in self.correlation_results['variable'] if any(x in col for x in ['score', 'total', 'diff'])],
            'Team stats': [col for col in self.correlation_results['variable'] if any(x in col for x in ['home_', 'away_'])],
            'Player stats': [col for col in self.correlation_results['variable'] if any(x in col for x in ['qb_', 'rushing', 'receiving', 'touchdowns'])],
            'Injury-related': [col for col in self.correlation_results['variable'] if 'injur' in col],
            'Game context': [col for col in self.correlation_results['variable'] if any(x in col for x in ['rest', 'divisional', 'conference', 'temp', 'wind'])]
        }
        
        for category, vars_list in categories.items():
            if vars_list:
                print(f"{category}: {len(vars_list)} variables")
        
        return self.correlation_results
    
    def save_results(self, filename: str = "nfl_correlation_results.csv"):
        """Save correlation results to CSV"""
        if not self.correlation_results.empty:
            self.correlation_results.to_csv(filename, index=False)
            print(f"\nResults saved to {filename}")
    
    def create_visualizations(self):
        """Create interactive correlation visualizations with scrolling"""
        if self.correlation_results.empty:
            print("No correlation results available for visualization.")
            return
        
        if PLOTLY_AVAILABLE:
            self._create_interactive_plots()
        else:
            self._create_static_plots()
    
    def _create_interactive_plots(self):
        """Create interactive plots with scrolling using Plotly"""
        print("Creating interactive visualizations with scrolling...")
        
        # Create subplots with scrolling capability
        fig = make_subplots(
            rows=2, cols=2,
            subplot_titles=('Top Correlations (Scrollable)', 'Correlation Distribution', 
                          'P-value Distribution', 'Correlation vs P-value Scatter'),
            specs=[[{"secondary_y": False}, {"secondary_y": False}],
                   [{"secondary_y": False}, {"secondary_y": False}]]
        )
        
        # 1. Top correlations bar plot (scrollable)
        top_50 = self.correlation_results.head(50)  # Show top 50 for scrolling
        fig.add_trace(
            go.Bar(
                y=top_50['variable'],
                x=top_50['spearman_correlation'],
                orientation='h',
                text=[f"{corr:.3f}" for corr in top_50['spearman_correlation']],
                textposition='auto',
                hovertemplate='<b>%{y}</b><br>Correlation: %{x:.3f}<br>P-value: %{customdata[0]:.4f}<br>Sample Size: %{customdata[1]}<extra></extra>',
                customdata=list(zip(top_50['spearman_p_value'], top_50['sample_size'])),
                marker_color=['red' if p < 0.001 else 'orange' if p < 0.01 else 'yellow' if p < 0.05 else 'lightblue' 
                             for p in top_50['spearman_p_value']],
                name='Correlations'
            ),
            row=1, col=1
        )
        
        # 2. Correlation strength distribution
        fig.add_trace(
            go.Histogram(
                x=self.correlation_results['spearman_correlation'],
                nbinsx=30,
                opacity=0.7,
                name='Correlation Distribution',
                hovertemplate='Range: %{x}<br>Count: %{y}<extra></extra>'
            ),
            row=1, col=2
        )
        
        # 3. P-value distribution with significance lines
        fig.add_trace(
            go.Histogram(
                x=self.correlation_results['spearman_p_value'],
                nbinsx=30,
                opacity=0.7,
                name='P-value Distribution',
                hovertemplate='P-value: %{x}<br>Count: %{y}<extra></extra>'
            ),
            row=2, col=1
        )
        
        # Add significance lines
        for p_val, color, label in [(0.05, 'red', 'p=0.05'), (0.01, 'orange', 'p=0.01'), (0.001, 'green', 'p=0.001')]:
            fig.add_vline(x=p_val, line_dash="dash", line_color=color, 
                         annotation_text=label, row=2, col=1)
        
        # 4. Correlation vs P-value scatter
        fig.add_trace(
            go.Scatter(
                x=self.correlation_results['spearman_p_value'],
                y=self.correlation_results['spearman_correlation'],
                mode='markers',
                marker=dict(
                    size=8,
                    color=self.correlation_results['sample_size'],
                    colorscale='Viridis',
                    showscale=True,
                    colorbar=dict(title="Sample Size"),
                    opacity=0.6
                ),
                text=self.correlation_results['variable'],
                hovertemplate='<b>%{text}</b><br>Correlation: %{y:.3f}<br>P-value: %{x:.4f}<br>Sample Size: %{marker.color}<extra></extra>',
                name='Variables'
            ),
            row=2, col=2
        )
        
        # Update layout for scrolling and interactivity
        fig.update_layout(
            title={
                'text': 'NFL Data Correlation Analysis - Interactive Dashboard',
                'x': 0.5,
                'xanchor': 'center',
                'font': {'size': 20}
            },
            height=800,
            showlegend=False,
            # Enable scrolling on the correlation bar chart
            yaxis=dict(
                title="Variables",
                tickfont=dict(size=10),
                # This enables scrolling on the y-axis for the bar chart
                fixedrange=False
            ),
            yaxis2=dict(title="Frequency"),
            yaxis3=dict(title="Frequency"),
            yaxis4=dict(title="Spearman Correlation"),
            xaxis=dict(title="Spearman Correlation"),
            xaxis2=dict(title="Correlation Value"),
            xaxis3=dict(title="P-value", type="log"),
            xaxis4=dict(title="P-value (log scale)", type="log"),
            # Enable zoom and pan
            dragmode='pan'
        )
        
        # Make the first subplot scrollable by setting a fixed height
        fig.update_layout(
            yaxis=dict(
                fixedrange=False,  # Allow scrolling
                autorange=False,
                range=[-0.5, 19.5]  # Show first 20 items initially
            )
        )
        
        # Save as HTML for interactive viewing
        fig.write_html('nfl_correlation_interactive.html')
        print("Interactive visualization saved as 'nfl_correlation_interactive.html'")
        
        # Also create a separate scrollable table
        self._create_scrollable_table()
        
        # Don't show in terminal, just save HTML files
        print("Interactive plots saved as HTML files. Open 'nfl_correlation_interactive.html' in your browser to view.")
    
    def _create_scrollable_table(self):
        """Create a scrollable data table of all correlations"""
        # Create a comprehensive table with all correlations
        table_data = self.correlation_results.copy()
        
        # Add significance indicators
        table_data['significance'] = table_data['spearman_p_value'].apply(
            lambda x: '***' if x < 0.001 else '**' if x < 0.01 else '*' if x < 0.05 else ''
        )
        
        # Format numbers for better display
        table_data['pearson_formatted'] = table_data['pearson_correlation'].round(4)
        table_data['spearman_formatted'] = table_data['spearman_correlation'].round(4)
        table_data['p_value_formatted'] = table_data['spearman_p_value'].round(6)
        
        # Create interactive table
        fig = go.Figure(data=[go.Table(
            header=dict(
                values=['Variable', 'Spearman Correlation', 'Pearson Correlation', 
                       'P-value', 'Significance', 'Sample Size', 'Mean', 'Std Dev'],
                fill_color='lightblue',
                align='left',
                font=dict(size=12, color='black')
            ),
            cells=dict(
                values=[
                    table_data['variable'],
                    table_data['spearman_formatted'],
                    table_data['pearson_formatted'],
                    table_data['p_value_formatted'],
                    table_data['significance'],
                    table_data['sample_size'],
                    table_data['mean_value'].round(2),
                    table_data['std_value'].round(2)
                ],
                fill_color='white',
                align='left',
                font=dict(size=11)
            )
        )])
        
        fig.update_layout(
            title='Complete NFL Correlation Results - Scrollable Table',
            height=600,
            margin=dict(l=10, r=10, t=50, b=10)
        )
        
        fig.write_html('nfl_correlation_table.html')
        print("Scrollable table saved as 'nfl_correlation_table.html'")
    
    def _create_static_plots(self):
        """Create static plots using matplotlib (fallback)"""
        print("Creating static visualizations...")
        
        # Set up the plotting style
        plt.style.use('default')
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        fig.suptitle('NFL Data Correlation Analysis', fontsize=16, fontweight='bold')
        
        # 1. Top correlations bar plot
        top_20 = self.correlation_results.head(20)
        axes[0, 0].barh(range(len(top_20)), top_20['spearman_correlation'])
        axes[0, 0].set_yticks(range(len(top_20)))
        axes[0, 0].set_yticklabels(top_20['variable'], fontsize=8)
        axes[0, 0].set_xlabel('Spearman Correlation')
        axes[0, 0].set_title('Top 20 Correlations with Home Win')
        axes[0, 0].grid(True, alpha=0.3)
        
        # 2. Correlation strength distribution
        axes[0, 1].hist(self.correlation_results['spearman_correlation'], bins=30, alpha=0.7, edgecolor='black')
        axes[0, 1].set_xlabel('Spearman Correlation')
        axes[0, 1].set_ylabel('Frequency')
        axes[0, 1].set_title('Distribution of Correlation Strengths')
        axes[0, 1].grid(True, alpha=0.3)
        
        # 3. P-value distribution
        axes[1, 0].hist(self.correlation_results['spearman_p_value'], bins=30, alpha=0.7, edgecolor='black')
        axes[1, 0].axvline(x=0.05, color='red', linestyle='--', label='p=0.05')
        axes[1, 0].axvline(x=0.01, color='orange', linestyle='--', label='p=0.01')
        axes[1, 0].axvline(x=0.001, color='green', linestyle='--', label='p=0.001')
        axes[1, 0].set_xlabel('P-value')
        axes[1, 0].set_ylabel('Frequency')
        axes[1, 0].set_title('Distribution of P-values')
        axes[1, 0].legend()
        axes[1, 0].grid(True, alpha=0.3)
        
        # 4. Correlation vs P-value scatter
        scatter = axes[1, 1].scatter(self.correlation_results['spearman_p_value'], 
                                   self.correlation_results['spearman_correlation'],
                                   c=self.correlation_results['sample_size'], 
                                   cmap='viridis', alpha=0.6)
        axes[1, 1].set_xlabel('P-value (log scale)')
        axes[1, 1].set_ylabel('Spearman Correlation')
        axes[1, 1].set_title('Correlation vs P-value')
        axes[1, 1].set_xscale('log')
        axes[1, 1].grid(True, alpha=0.3)
        plt.colorbar(scatter, ax=axes[1, 1], label='Sample Size')
        
        plt.tight_layout()
        plt.savefig('nfl_correlation_analysis.png', dpi=300, bbox_inches='tight')
        plt.show()
        
        print("Static visualizations saved as 'nfl_correlation_analysis.png'")

def main():
    """Main execution function"""
    print("Starting NFL Correlation Analysis...")
    
    # Initialize analyzer with fewer seasons to avoid data loading issues
    analyzer = NFLCorrelationAnalyzer(seasons=[2023, 2024])
    
    # Load all data
    analyzer.load_all_data()
    
    # Create comprehensive dataset
    game_dataset = analyzer.create_game_level_dataset()
    
    # Calculate correlations
    correlation_results = analyzer.calculate_correlations()
    
    # Generate report
    analyzer.generate_report()
    
    # Save results
    analyzer.save_results()
    
    # Create visualizations (will work without plotly)
    analyzer.create_visualizations()
    
    print("\nCorrelation analysis complete!")
    return analyzer

if __name__ == "__main__":
    analyzer = main()
