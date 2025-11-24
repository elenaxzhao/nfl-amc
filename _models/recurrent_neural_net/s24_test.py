#!/usr/bin/env python3
"""
RNN Model Test: 2024 Season Evaluation
========================================

Rolling origin evaluation on 2024 season weeks 6-18.
Train on: Prior seasons + weeks 1-(w-1)
Test on: Week w

Uses strict time-aware splits with no look-ahead.
"""

import sys
import os
import yaml
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional
import warnings
warnings.filterwarnings('ignore')

# Add current directory to path to import modules
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from data_loader import NFLDataLoader, TimeAwareSplitter
from feature_engineering import FeatureEngineer
from sequence_builder import SequenceBuilder, RecencyWeightCalculator
from rnn_model import NFLGamePredictor, create_model_from_config
from trainer import Trainer, TrainingConfig
from metrics import MetricsCalculator, ResultsLogger, Calibrator


class NFLDataset(Dataset):
    """PyTorch dataset for NFL games."""
    
    def __init__(self, data: List[Dict]):
        self.data = data
    
    def __len__(self):
        return len(self.data)
    
    def __getitem__(self, idx):
        return self.data[idx]


def collate_fn(batch: List[Dict]) -> Dict:
    """Collate function for data loader."""
    from sequence_builder import SequenceBuilder
    builder = SequenceBuilder()
    return builder.collate_batch(batch)


def load_config(config_path: str = "config.yaml") -> Dict:
    """Load configuration from YAML."""
    with open(config_path, 'r') as f:
        config = yaml.safe_load(f)
    return config


def set_seed(seed: int):
    """Set random seeds for reproducibility."""
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    np.random.seed(seed)
    import random
    random.seed(seed)
    
    if torch.cuda.is_available():
        torch.backends.cudnn.deterministic = True
        torch.backends.cudnn.benchmark = False


def train_model_for_week(
    config: Dict,
    train_games: pd.DataFrame,
    val_games: pd.DataFrame,
    team_features: pd.DataFrame,
    feature_columns: List[str],
    team_to_id: Dict[str, int],
    season: int,
    week: int,
    verbose: bool = True
) -> Tuple[nn.Module, Dict]:
    """
    Train model for a specific week.
    
    Args:
        config: Configuration dictionary
        train_games: Training games
        val_games: Validation games
        team_features: Team features dataframe
        feature_columns: List of feature column names
        team_to_id: Team name to ID mapping
        season: Current season
        week: Current week
        verbose: Print progress
        
    Returns:
        (trained_model, training_history)
    """
    if verbose:
        print(f"\n{'='*70}")
        print(f"Training model for {season} Week {week}")
        print(f"{'='*70}")
        print(f"Training games: {len(train_games)}")
        print(f"Validation games: {len(val_games)}")
    
    # Build sequences
    seq_config = config['sequence']
    builder = SequenceBuilder(
        max_history_length=seq_config['max_history_length'],
        min_history_length=seq_config['min_history_length'],
        pad_value=seq_config['pad_value'],
        feature_columns=feature_columns
    )
    
    # Build datasets
    if verbose:
        print(f"\nBuilding training sequences...")
    
    train_dataset = []
    for _, game in train_games.iterrows():
        home_team = game['home_team']
        away_team = game['away_team']
        
        if pd.isna(home_team) or pd.isna(away_team):
            continue
        
        # Get sequences
        home_seq, away_seq = builder.build_matchup_sequences(
            home_team, away_team, team_features,
            up_to_date=(game['season'], game['week'])
        )
        
        if home_seq is None or away_seq is None:
            continue
        
        train_dataset.append({
            'game_id': game['game_id'],
            'season': game['season'],
            'week': game['week'],
            'home_team': home_team,
            'away_team': away_team,
            'home_sequence': home_seq.features,
            'home_mask': home_seq.mask,
            'away_sequence': away_seq.features,
            'away_mask': away_seq.mask,
            'is_divisional': 0.0,
            'home_score': float(game['home_score']),
            'away_score': float(game['away_score']),
            'home_win': 1.0 if game['home_score'] > game['away_score'] else 0.0,
            'home_team_id': team_to_id.get(home_team, 0),
            'away_team_id': team_to_id.get(away_team, 0)
        })
    
    # Build validation dataset
    if verbose:
        print(f"Building validation sequences...")
    
    val_dataset = []
    for _, game in val_games.iterrows():
        home_team = game['home_team']
        away_team = game['away_team']
        
        if pd.isna(home_team) or pd.isna(away_team):
            continue
        
        if pd.isna(game.get('home_score')) or pd.isna(game.get('away_score')):
            continue
        
        home_seq, away_seq = builder.build_matchup_sequences(
            home_team, away_team, team_features,
            up_to_date=(game['season'], game['week'])
        )
        
        if home_seq is None or away_seq is None:
            continue
        
        val_dataset.append({
            'game_id': game['game_id'],
            'season': game['season'],
            'week': game['week'],
            'home_team': home_team,
            'away_team': away_team,
            'home_sequence': home_seq.features,
            'home_mask': home_seq.mask,
            'away_sequence': away_seq.features,
            'away_mask': away_seq.mask,
            'is_divisional': 0.0,
            'home_score': float(game['home_score']),
            'away_score': float(game['away_score']),
            'home_win': 1.0 if game['home_score'] > game['away_score'] else 0.0,
            'home_team_id': team_to_id.get(home_team, 0),
            'away_team_id': team_to_id.get(away_team, 0)
        })
    
    if verbose:
        print(f"✓ Training samples: {len(train_dataset)}")
        print(f"✓ Validation samples: {len(val_dataset)}")
    
    if len(train_dataset) == 0:
        print("ERROR: No training data")
        return None, None
    
    # Create data loaders
    train_loader = DataLoader(
        NFLDataset(train_dataset),
        batch_size=config['training']['batch_size'],
        shuffle=True,
        collate_fn=collate_fn
    )
    
    val_loader = None
    if len(val_dataset) > 0:
        val_loader = DataLoader(
            NFLDataset(val_dataset),
            batch_size=config['training']['batch_size'],
            shuffle=False,
            collate_fn=collate_fn
        )
    
    # Create model
    feature_dim = len(feature_columns)
    model_config = config['model'].copy()
    model_config['feature_dim'] = feature_dim
    
    model = NFLGamePredictor(
        feature_dim=feature_dim,
        team_embedding_dim=model_config.get('team_embedding_dim', 32),
        hidden_dim=model_config.get('hidden_dim', 128),
        num_layers=model_config.get('num_layers', 2),
        dropout=model_config.get('dropout', 0.3),
        encoder_type=model_config.get('encoder_type', 'gru'),
        bidirectional=model_config.get('bidirectional', False),
        win_head_hidden=model_config.get('win_head_hidden', [64, 32]),
        score_head_hidden=model_config.get('score_head_hidden', [64, 32]),
        num_teams=len(team_to_id)
    )
    
    # Create trainer
    train_config = TrainingConfig(
        learning_rate=config['training']['learning_rate'],
        batch_size=config['training']['batch_size'],
        max_epochs=config['training']['max_epochs'],
        weight_decay=config['training']['weight_decay'],
        gradient_clip_value=config['training']['gradient_clip_value'],
        use_amp=config['training']['use_amp'],
        early_stopping_patience=config['training']['early_stopping_patience'],
        early_stopping_metric=config['training']['early_stopping_metric'],
        early_stopping_mode=config['training']['early_stopping_mode'],
        recency_half_life=config['training']['recency_half_life'],
        apply_recency_as=config['training']['apply_recency_as'],
        lambda_bce=config['loss']['lambda_bce'],
        lambda_score=config['loss']['lambda_score'],
        score_loss_type=config['loss']['score_loss_type']
    )
    
    trainer = Trainer(model, train_config)
    
    # Train
    history = trainer.train(train_loader, val_loader, verbose=verbose)
    
    return trainer.model, history


def predict_week(
    model: nn.Module,
    test_games: pd.DataFrame,
    team_features: pd.DataFrame,
    feature_columns: List[str],
    team_to_id: Dict[str, int],
    season: int,
    week: int,
    device: torch.device
) -> pd.DataFrame:
    """
    Make predictions for a week.
    
    Returns:
        DataFrame with predictions
    """
    builder = SequenceBuilder(
        max_history_length=10,
        feature_columns=feature_columns
    )
    
    predictions = []
    
    model.eval()
    with torch.no_grad():
        for _, game in test_games.iterrows():
            home_team = game['home_team']
            away_team = game['away_team']
            
            if pd.isna(home_team) or pd.isna(away_team):
                continue
            
            # Build sequences
            home_seq, away_seq = builder.build_matchup_sequences(
                home_team, away_team, team_features,
                up_to_date=(season, week)
            )
            
            if home_seq is None or away_seq is None:
                continue
            
            # Prepare inputs
            home_sequences = torch.FloatTensor(home_seq.features).unsqueeze(0).to(device)
            away_sequences = torch.FloatTensor(away_seq.features).unsqueeze(0).to(device)
            home_masks = torch.FloatTensor(home_seq.mask).unsqueeze(0).to(device)
            away_masks = torch.FloatTensor(away_seq.mask).unsqueeze(0).to(device)
            is_divisional = torch.FloatTensor([0.0]).to(device)
            home_team_id = torch.LongTensor([team_to_id.get(home_team, 0)]).to(device)
            away_team_id = torch.LongTensor([team_to_id.get(away_team, 0)]).to(device)
            
            # Predict
            outputs = model(
                home_sequences=home_sequences,
                away_sequences=away_sequences,
                home_masks=home_masks,
                away_masks=away_masks,
                home_team_ids=home_team_id,
                away_team_ids=away_team_id,
                is_divisional=is_divisional
            )
            
            # Store prediction
            pred = {
                'game_id': game['game_id'],
                'season': season,
                'week': week,
                'home_team': home_team,
                'away_team': away_team,
                'y_pred_win_prob': outputs['win_probs'].item(),
                'y_pred_home_score': outputs['home_scores'].item(),
                'y_pred_away_score': outputs['away_scores'].item(),
            }
            
            # Add actuals if available
            if pd.notna(game.get('home_score')) and pd.notna(game.get('away_score')):
                pred['y_true_home_score'] = float(game['home_score'])
                pred['y_true_away_score'] = float(game['away_score'])
                pred['y_true_win'] = 1.0 if game['home_score'] > game['away_score'] else 0.0
            
            predictions.append(pred)
    
    return pd.DataFrame(predictions)


def main():
    """Run 2024 season evaluation."""
    print("\n" + "="*35)
    print("RNN MODEL - 2024 SEASON EVALUATION")
    print("="*35 + "\n")
    
    # Load config
    print("Loading configuration...")
    config = load_config("config.yaml")
    
    # Set seed for reproducibility
    seed = config['training'].get('seed', 42)
    set_seed(seed)
    print(f"✓ Random seed set to {seed}")
    
    # Create results directory
    results_dir = config['logging']['results_dir']
    Path(results_dir).mkdir(parents=True, exist_ok=True)
    
    # Load data
    print("\n" + "="*70)
    print("LOADING DATA")
    print("="*70)
    
    data_config = config['data']
    loader = NFLDataLoader(
        seasons=data_config['seasons'],
        cache_dir=data_config['cache_dir'],
        use_cache=data_config['use_cache']
    )
    
    pbp, games = loader.load_data()
    
    # Get team list and create ID mapping
    teams = loader.get_all_teams()
    team_to_id = {team: idx for idx, team in enumerate(teams)}
    print(f"✓ Found {len(teams)} teams")
    
    # Feature engineering
    print("\n" + "="*70)
    print("FEATURE ENGINEERING")
    print("="*70)
    
    feature_config = config['features']
    engineer = FeatureEngineer(
        use_epa=feature_config['use_epa'],
        use_success_rate=feature_config['use_success_rate'],
        use_turnover_rate=feature_config['use_turnover_rate'],
        use_pace=feature_config['use_pace'],
        use_drives=feature_config['use_drives'],
        use_elo=feature_config['use_elo']
    )
    
    print("Computing features for all games...")
    all_completed_games = games[
        (games['game_type'] == 'REG') &
        (games['home_score'].notna())
    ]
    
    team_features_all = engineer.compute_features_for_games(all_completed_games, pbp)
    team_features_all = engineer.update_elo_ratings(team_features_all)
    
    feature_columns = engineer.get_feature_names()
    print(f"✓ Computed {len(feature_columns)} features per game")
    print(f"  Features: {feature_columns}")
    
    # Rolling origin evaluation
    print("\n" + "="*70)
    print("ROLLING ORIGIN EVALUATION - 2024 SEASON")
    print("="*70)
    
    test_season = config['rolling_origin']['test_season']
    test_weeks = config['rolling_origin']['test_weeks']
    
    all_predictions = []
    weekly_results = []
    
    for week in test_weeks:
        print(f"\n{'─'*70}")
        print(f"WEEK {week}")
        print(f"{'─'*70}")
        
        # Get train/test split
        train_games, test_games = TimeAwareSplitter.get_train_test_split(
            games, test_season=test_season, test_week=week
        )
        
        # Create validation split from training data
        train_games, val_games = TimeAwareSplitter.create_validation_split(
            train_games,
            val_fraction=config['training']['val_split'],
            time_ordered=True
        )
        
        print(f"  Train: {len(train_games)} games")
        print(f"  Val: {len(val_games)} games")
        print(f"  Test: {len(test_games)} games")
        
        # Train model
        model, history = train_model_for_week(
            config=config,
            train_games=train_games,
            val_games=val_games,
            team_features=team_features_all,
            feature_columns=feature_columns,
            team_to_id=team_to_id,
            season=test_season,
            week=week,
            verbose=True
        )
        
        if model is None:
            print(f"  Skipping week {week} (no training data)")
            continue
        
        # Predict on test week
        print(f"\nPredicting Week {week}...")
        
        device = next(model.parameters()).device
        week_predictions = predict_week(
            model=model,
            test_games=test_games,
            team_features=team_features_all,
            feature_columns=feature_columns,
            team_to_id=team_to_id,
            season=test_season,
            week=week,
            device=device
        )
        
        print(f"✓ Generated {len(week_predictions)} predictions")
        
        # Evaluate if results available
        if 'y_true_win' in week_predictions.columns:
            metrics = MetricsCalculator.compute_all_metrics(
                y_true_win=week_predictions['y_true_win'].values,
                y_pred_win_prob=week_predictions['y_pred_win_prob'].values,
                y_true_home_score=week_predictions['y_true_home_score'].values,
                y_true_away_score=week_predictions['y_true_away_score'].values,
                y_pred_home_score=week_predictions['y_pred_home_score'].values,
                y_pred_away_score=week_predictions['y_pred_away_score'].values
            )
            
            metrics['week'] = week
            metrics['n_games'] = len(week_predictions)
            weekly_results.append(metrics)
            
            print(f"\n  Week {week} Results:")
            print(f"    Accuracy: {metrics['accuracy']:.1%}")
            print(f"    Log Loss: {metrics['log_loss']:.4f}")
            print(f"    Brier Score: {metrics['brier_score']:.4f}")
            print(f"    MAE: {metrics['mae_total']:.2f} points")
            print(f"    Spread MAE: {metrics['spread_mae']:.2f} points")
        
        all_predictions.append(week_predictions)
    
    # Combine all predictions
    print("\n" + "="*70)
    print("OVERALL RESULTS")
    print("="*70)
    
    all_predictions_df = pd.concat(all_predictions, ignore_index=True)
    weekly_results_df = pd.DataFrame(weekly_results)
    
    # Overall metrics
    if len(weekly_results_df) > 0:
        overall_metrics = MetricsCalculator.compute_all_metrics(
            y_true_win=all_predictions_df['y_true_win'].values,
            y_pred_win_prob=all_predictions_df['y_pred_win_prob'].values,
            y_true_home_score=all_predictions_df['y_true_home_score'].values,
            y_true_away_score=all_predictions_df['y_true_away_score'].values,
            y_pred_home_score=all_predictions_df['y_pred_home_score'].values,
            y_pred_away_score=all_predictions_df['y_pred_away_score'].values
        )
        
        logger = ResultsLogger(results_dir=results_dir)
        logger.log_metrics(overall_metrics, prefix="Overall ")
        
        # Save results
        print("\n" + "="*70)
        print("SAVING RESULTS")
        print("="*70)
        
        logger.save_predictions(all_predictions_df, "rnn_2024_predictions.csv")
        logger.save_weekly_metrics(weekly_results_df, "rnn_2024_weekly_metrics.csv")
        logger.save_metrics(overall_metrics, "rnn_2024_summary.csv")
        
        # Print weekly breakdown
        print("\n" + "="*70)
        print("WEEKLY BREAKDOWN")
        print("="*70)
        print(f"\n{'Week':<6} {'Games':<7} {'Accuracy':<12} {'Log Loss':<12} {'MAE':<10}")
        print("─"*70)
        
        for _, row in weekly_results_df.iterrows():
            print(f"{row['week']:<6} {row['n_games']:<7} "
                  f"{row['accuracy']:>10.1%}  "
                  f"{row['log_loss']:>10.4f}  "
                  f"{row['mae_total']:>8.2f}")
        
        print("─"*70)
        print(f"{'Total':<6} {weekly_results_df['n_games'].sum():<7} "
              f"{overall_metrics['accuracy']:>10.1%}  "
              f"{overall_metrics['log_loss']:>10.4f}  "
              f"{overall_metrics['mae_total']:>8.2f}")
    
    print("\n" + "="*70)
    print("EVALUATION COMPLETE")
    print("="*70 + "\n")


if __name__ == "__main__":
    main()

