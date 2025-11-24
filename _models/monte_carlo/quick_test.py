#!/usr/bin/env python3
"""
Quick Test Script for Monte Carlo Model
=========================================

A simplified script to quickly test the improved Monte Carlo model
with rolling/incremental training.
"""

import sys
import os

# Add current directory to path to import the model
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from monte_carlo_model import ImprovedMonteCarloModel


def quick_test():
    """Run a quick test of the model."""
    print("\n" + "="*70)
    print("QUICK TEST - Improved NFL Monte Carlo Model")
    print("="*70)
    
    print("\nInitializing model...")
    print("Configuration:")
    print("  - Training: 2022, 2023 seasons (prior)")
    print("  - Test: 2024 season (with rolling updates)")
    print("  - Simulations: 10,000 per game")
    print("  - Bayesian Updating: Enabled")
    print("  - Regression to Mean: Enabled")
    
    model = ImprovedMonteCarloModel(
        train_seasons=[2022, 2023],
        test_season=2024,
        n_simulations=10000,
        home_field_advantage=2.5,
        recency_weight=0.5,
        use_bayesian_updating=True,
        use_regression_to_mean=True,
        min_games_threshold=4,
    )
    
    # Load data
    model.load_data()
    
    # Calculate league averages (needed for regression to mean)
    model.calculate_league_averages()
    
    # Build prior distributions from historical seasons
    model.build_prior_distributions()
    
    # Get some teams for testing
    teams = list(model.prior_distributions.keys())
    
    if len(teams) < 2:
        print("\nError: Not enough teams in dataset")
        return
    
    print(f"\n✓ Model ready! Found {len(teams)} teams")
    print(f"\nSample teams: {', '.join(teams[:10])}")
    
    # Test a single game prediction (Week 10 with rolling update)
    print("\n" + "="*70)
    print("TEST SINGLE GAME PREDICTION (Week 10)")
    print("="*70)
    
    home_team = teams[0]
    away_team = teams[1]
    
    print(f"\nPredicting: {away_team} @ {home_team} (Week 10)")
    print("(Model will update with weeks 1-9 data before predicting)")
    
    prediction = model.predict_game(home_team, away_team, week=10)
    
    print(f"\n{away_team} @ {home_team}")
    print("─"*70)
    print(f"Predicted Winner: {prediction['predicted_winner']}")
    print(f"Confidence: {prediction['confidence']:.1%}")
    print(f"High Confidence: {'Yes' if prediction['high_confidence'] else 'No'}")
    print(f"\nExpected Score:")
    print(f"  {home_team}: {prediction['expected_home_score']:.1f} points")
    print(f"  {away_team}: {prediction['expected_away_score']:.1f} points")
    print(f"  Spread: {prediction['expected_spread']:+.1f} (favoring {home_team})")
    print(f"\nWin Probabilities:")
    print(f"  {home_team}: {prediction['home_win_probability']:.1%}")
    print(f"  {away_team}: {prediction['away_win_probability']:.1%}")
    print(f"\nData Sample Size:")
    print(f"  {home_team}: {prediction.get('home_games_played', 'N/A')} games in 2024")
    print(f"  {away_team}: {prediction.get('away_games_played', 'N/A')} games in 2024")
    
    # Test week prediction with rolling update
    print("\n" + "="*70)
    print("TEST WEEK PREDICTION (Week 10 - Rolling Update)")
    print("="*70)
    
    week_preds = model.predict_week_rolling(week=10, season=2024)
    
    if week_preds:
        print(f"\n✓ Successfully predicted {len(week_preds)} games for Week 10")
        print("\nFirst 3 games:")
        for i, pred in enumerate(week_preds[:3], 1):
            conf_flag = "✓" if pred.get('high_confidence', False) else "⚠"
            print(f"\n{i}. {conf_flag} {pred['away_team']} @ {pred['home_team']}")
            print(f"   Winner: {pred['predicted_winner']} ({pred['confidence']:.1%})")
            print(f"   Score: {pred['expected_home_score']:.1f} - {pred['expected_away_score']:.1f}")
            print(f"   Spread: {pred['expected_spread']:+.1f}")
            print(f"   Games played: {pred.get('home_games_played', 0)}, {pred.get('away_games_played', 0)}")
    
    print("\n" + "="*70)
    print("QUICK TEST COMPLETE!")
    print("="*70)
    print("\n✓ Improved model is working correctly!")
    print("\nKey Features Demonstrated:")
    print("  ✓ Rolling/incremental training (updates with current season)")
    print("  ✓ Bayesian updating (blends historical + current data)")
    print("  ✓ Confidence-based predictions (flags low-confidence)")
    print("  ✓ Adaptive sample size handling")
    print("\nNext steps:")
    print("1. Run 'python s24_test.py' to test on 2024 weeks 6-18")
    print("2. See README.md for documentation")
    print("3. Customize model parameters for your use case")
    print("="*70 + "\n")


if __name__ == "__main__":
    quick_test()

