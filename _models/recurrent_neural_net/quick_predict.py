#!/usr/bin/env python3
"""
Quick Prediction Script
========================

Simple interface for making predictions on upcoming games.
"""

import yaml
import torch
import pandas as pd
from pathlib import Path

from .data_loader import NFLDataLoader
from .feature_engineering import FeatureEngineer
from .sequence_builder import SequenceBuilder
from .rnn_model import NFLGamePredictor


def load_latest_model(checkpoint_dir: str = "checkpoints/") -> torch.nn.Module:
    """Load the most recent model checkpoint."""
    checkpoint_path = Path(checkpoint_dir)
    
    if not checkpoint_path.exists():
        raise FileNotFoundError(f"No checkpoints found in {checkpoint_dir}")
    
    # Find latest checkpoint
    checkpoints = list(checkpoint_path.glob("*.pt"))
    if not checkpoints:
        raise FileNotFoundError(f"No .pt files found in {checkpoint_dir}")
    
    latest = max(checkpoints, key=lambda p: p.stat().st_mtime)
    
    print(f"Loading model from {latest}")
    checkpoint = torch.load(latest, map_location='cpu')
    
    # Reconstruct model
    # This assumes model config is saved in checkpoint
    model_config = checkpoint.get('model_config', {})
    
    model = NFLGamePredictor(**model_config)
    model.load_state_dict(checkpoint['model_state_dict'])
    model.eval()
    
    return model


def predict_game(
    model: torch.nn.Module,
    home_team: str,
    away_team: str,
    season: int,
    week: int,
    team_features: pd.DataFrame,
    feature_columns: list,
    team_to_id: dict,
    device: torch.device = torch.device('cpu')
) -> dict:
    """
    Predict a single game.
    
    Args:
        model: Trained model
        home_team: Home team abbreviation
        away_team: Away team abbreviation
        season: Current season
        week: Current week
        team_features: Historical team features
        feature_columns: List of feature names
        team_to_id: Team name to ID mapping
        device: Device to run on
        
    Returns:
        Dictionary with predictions
    """
    builder = SequenceBuilder(
        max_history_length=10,
        feature_columns=feature_columns
    )
    
    # Build sequences
    home_seq, away_seq = builder.build_matchup_sequences(
        home_team, away_team, team_features,
        up_to_date=(season, week)
    )
    
    if home_seq is None or away_seq is None:
        return {
            'error': 'Insufficient historical data',
            'home_team': home_team,
            'away_team': away_team
        }
    
    # Prepare tensors
    home_sequences = torch.FloatTensor(home_seq.features).unsqueeze(0).to(device)
    away_sequences = torch.FloatTensor(away_seq.features).unsqueeze(0).to(device)
    home_masks = torch.FloatTensor(home_seq.mask).unsqueeze(0).to(device)
    away_masks = torch.FloatTensor(away_seq.mask).unsqueeze(0).to(device)
    
    home_team_id = torch.LongTensor([team_to_id.get(home_team, 0)]).to(device)
    away_team_id = torch.LongTensor([team_to_id.get(away_team, 0)]).to(device)
    is_divisional = torch.FloatTensor([0.0]).to(device)
    
    # Predict
    model.eval()
    with torch.no_grad():
        outputs = model(
            home_sequences=home_sequences,
            away_sequences=away_sequences,
            home_masks=home_masks,
            away_masks=away_masks,
            home_team_ids=home_team_id,
            away_team_ids=away_team_id,
            is_divisional=is_divisional
        )
    
    # Format results
    win_prob = outputs['win_probs'].item()
    home_score = outputs['home_scores'].item()
    away_score = outputs['away_scores'].item()
    
    return {
        'home_team': home_team,
        'away_team': away_team,
        'home_win_probability': win_prob,
        'away_win_probability': 1 - win_prob,
        'predicted_winner': home_team if win_prob > 0.5 else away_team,
        'confidence': max(win_prob, 1 - win_prob),
        'predicted_home_score': home_score,
        'predicted_away_score': away_score,
        'predicted_spread': home_score - away_score,
        'home_games_in_history': home_seq.seq_length,
        'away_games_in_history': away_seq.seq_length
    }


def main():
    """Demo: Predict upcoming games."""
    print("\n" + "="*70)
    print("QUICK PREDICTION - DEMO")
    print("="*70)
    
    # Load config
    with open("config.yaml", 'r') as f:
        config = yaml.safe_load(f)
    
    # Load data
    print("\nLoading data...")
    loader = NFLDataLoader(
        seasons=config['data']['seasons'],
        cache_dir=config['data']['cache_dir'],
        use_cache=True
    )
    
    pbp, games = loader.load_data()
    
    # Get teams
    teams = loader.get_all_teams()
    team_to_id = {team: idx for idx, team in enumerate(teams)}
    
    # Compute features
    print("Computing features...")
    engineer = FeatureEngineer(
        use_epa=True,
        use_success_rate=True,
        use_turnover_rate=True,
        use_pace=True,
        use_drives=True,
        use_elo=True
    )
    
    all_games = games[
        (games['game_type'] == 'REG') &
        (games['home_score'].notna())
    ]
    
    team_features = engineer.compute_features_for_games(all_games, pbp)
    team_features = engineer.update_elo_ratings(team_features)
    
    feature_columns = engineer.get_feature_names()
    
    print(f"✓ Data loaded and features computed")
    
    # Example predictions
    # NOTE: You would need a trained model checkpoint for this to work
    # This is just a demonstration of the interface
    
    print("\n" + "="*70)
    print("EXAMPLE PREDICTIONS")
    print("="*70)
    print("\nNote: This requires a trained model checkpoint.")
    print("After training, predictions would look like:\n")
    
    # Mock prediction for demonstration
    example_games = [
        ("KC", "BUF", 2024, 10),
        ("SF", "DAL", 2024, 10),
        ("PHI", "NYG", 2024, 10),
    ]
    
    print(f"{'Matchup':<25} {'Winner':<8} {'Confidence':<12} {'Score':<15}")
    print("─"*70)
    
    # Mock predictions (would use actual model)
    mock_predictions = [
        ("KC @ BUF", "KC", 0.62, "24-21"),
        ("SF @ DAL", "SF", 0.71, "28-17"),
        ("PHI @ NYG", "PHI", 0.58, "23-20"),
    ]
    
    for matchup, winner, conf, score in mock_predictions:
        print(f"{matchup:<25} {winner:<8} {conf:>10.1%}  {score:<15}")
    
    print("\n" + "="*70)
    print("To use with a trained model:")
    print("  1. Train model using s24_test.py")
    print("  2. Save checkpoint in trainer.save_checkpoint()")
    print("  3. Load here with load_latest_model()")
    print("  4. Call predict_game() for each matchup")
    print("="*70 + "\n")


if __name__ == "__main__":
    main()

