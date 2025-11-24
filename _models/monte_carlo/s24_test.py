#!/usr/bin/env python3
"""
Monte Carlo Model Test: 2024 Season Weeks 6-18
================================================

Train on: 2019 - 2023 seasons
Test on: 2024 season, weeks 6-18

Produces accuracy metrics for each week and overall average.
"""

import sys
import os

# Add current directory to path to import the model
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from monte_carlo_model import ImprovedMonteCarloModel
import pandas as pd
import numpy as np
from typing import Dict, List


def test_2024_weeks_6_18():
    """
    Test the Monte Carlo model on 2024 weeks 6-18.
    
    Returns:
        Dictionary with results for each week and overall metrics
    """
    print("\n" + "="*70)
    print("MONTE CARLO MODEL - 2024 SEASON TEST (Weeks 6-18)")
    print("="*70)
    print("\nConfiguration:")
    print("  Training Seasons: 2019-2023")
    print("  Test Season: 2024")
    print("  Test Weeks: 6-18")
    print("  Simulations per game: 10,000")
    print("="*70)
    
    # Initialize model
    print("\nInitializing model...")
    model = ImprovedMonteCarloModel(
        train_seasons=[2019, 2020, 2021, 2022, 2023],
        test_season=2024,
        n_simulations=10000,
        home_field_advantage=2.5,
        recency_weight=0.3,
        use_bayesian_updating=True,
        use_regression_to_mean=True,
        min_games_threshold=4,
        confidence_threshold=0.70,
    )
    
    # Load data
    model.load_data()
    
    # Calculate league averages (needed for regression to mean)
    model.calculate_league_averages()
    
    # Build team distributions
    model.build_prior_distributions()
    
    print("\n✓ Model ready!")
    
    # Test weeks 6-18
    weeks_to_test = list(range(6, 19))  # 6 through 18 inclusive
    
    weekly_results = []
    all_game_results = []
    
    print("\n" + "="*70)
    print("TESTING WEEKS 6-18")
    print("="*70)
    
    for week in weeks_to_test:
        print(f"\n{'─'*70}")
        print(f"Week {week}")
        print(f"{'─'*70}")
        
        # Get predictions for this week
        predictions = model.predict_week_rolling(week=week, season=2024)
        
        if not predictions:
            print(f"  No games found for Week {week}")
            continue
        
        # Get actual games for this week
        actual_games = model.games_data[
            (model.games_data['season'] == 2024) &
            (model.games_data['week'] == week) &
            (model.games_data['game_type'] == 'REG')
        ]
        
        # Evaluate predictions
        evaluation = model.evaluate_predictions(predictions, actual_games)
        
        if evaluation['n_games'] == 0:
            print(f"  No completed games to evaluate for Week {week}")
            continue
        
        # Store results
        weekly_results.append({
            'week': week,
            'games': evaluation['n_games'],
            'accuracy': evaluation['accuracy'],
            'avg_spread_error': evaluation['avg_spread_error'],
            'avg_score_error_home': evaluation['avg_score_error_home'],
            'avg_score_error_away': evaluation['avg_score_error_away'],
            'avg_score_error': evaluation['avg_score_error'],
        })
        
        # Store individual game results
        all_game_results.extend(evaluation['results'])
        
        # Print week summary
        print(f"  Games: {evaluation['n_games']}")
        print(f"  Accuracy: {evaluation['accuracy']:.1%} "
              f"({int(evaluation['accuracy'] * evaluation['n_games'])}/{evaluation['n_games']} correct)")
        print(f"  Avg Spread Error: {evaluation['avg_spread_error']:.2f} points")
        print(f"  Avg Score Error: {evaluation['avg_score_error']:.2f} points")
        
        # Show some example predictions from this week
        if evaluation['results']:
            print(f"\n  Sample games:")
            for result in evaluation['results'][:3]:  # Show first 3
                status = "✓" if result['correct'] else "✗"
                print(f"    {status} {result['away_team']} @ {result['home_team']}: "
                      f"Predicted {result['predicted_winner']}, "
                      f"Actual {result['actual_winner']}")
    
    # Calculate overall statistics
    print("\n" + "="*70)
    print("OVERALL RESULTS")
    print("="*70)
    
    if not weekly_results:
        print("\nNo results to display.")
        return None
    
    results_df = pd.DataFrame(weekly_results)
    
    # Overall metrics
    total_games = results_df['games'].sum()
    overall_accuracy = np.average(results_df['accuracy'], weights=results_df['games'])
    overall_spread_error = np.average(results_df['avg_spread_error'], weights=results_df['games'])
    overall_score_error = np.average(results_df['avg_score_error'], weights=results_df['games'])
    
    print(f"\nTotal Games Evaluated: {total_games}")
    print(f"Overall Accuracy: {overall_accuracy:.1%}")
    print(f"Overall Avg Spread Error: {overall_spread_error:.2f} points")
    print(f"Overall Avg Score Error: {overall_score_error:.2f} points")
    
    # Best and worst weeks
    best_week = results_df.loc[results_df['accuracy'].idxmax()]
    worst_week = results_df.loc[results_df['accuracy'].idxmin()]
    
    print(f"\nBest Week: Week {best_week['week']} ({best_week['accuracy']:.1%} accuracy)")
    print(f"Worst Week: Week {worst_week['week']} ({worst_week['accuracy']:.1%} accuracy)")
    
    # Detailed weekly breakdown
    print("\n" + "="*70)
    print("WEEK-BY-WEEK BREAKDOWN")
    print("="*70)
    print(f"\n{'Week':<6} {'Games':<7} {'Accuracy':<12} {'Spread Error':<14} {'Score Error':<12}")
    print("─"*70)
    
    for _, row in results_df.iterrows():
        print(f"{row['week']:<6} {row['games']:<7} "
              f"{row['accuracy']:>10.1%}  "
              f"{row['avg_spread_error']:>12.2f}  "
              f"{row['avg_score_error']:>11.2f}")
    
    print("─"*70)
    print(f"{'Total':<6} {total_games:<7} "
          f"{overall_accuracy:>10.1%}  "
          f"{overall_spread_error:>12.2f}  "
          f"{overall_score_error:>11.2f}")
    
    # Statistical summary
    print("\n" + "="*70)
    print("STATISTICAL SUMMARY")
    print("="*70)
    
    print(f"\nAccuracy Statistics:")
    print(f"  Mean: {results_df['accuracy'].mean():.1%}")
    print(f"  Median: {results_df['accuracy'].median():.1%}")
    print(f"  Std Dev: {results_df['accuracy'].std():.1%}")
    print(f"  Min: {results_df['accuracy'].min():.1%} (Week {results_df.loc[results_df['accuracy'].idxmin(), 'week']})")
    print(f"  Max: {results_df['accuracy'].max():.1%} (Week {results_df.loc[results_df['accuracy'].idxmax(), 'week']})")
    
    print(f"\nSpread Error Statistics:")
    print(f"  Mean: {results_df['avg_spread_error'].mean():.2f} points")
    print(f"  Median: {results_df['avg_spread_error'].median():.2f} points")
    print(f"  Std Dev: {results_df['avg_spread_error'].std():.2f} points")
    print(f"  Min: {results_df['avg_spread_error'].min():.2f} points")
    print(f"  Max: {results_df['avg_spread_error'].max():.2f} points")
    
    # Confusion matrix style summary
    if all_game_results:
        correct_predictions = sum(1 for r in all_game_results if r['correct'])
        incorrect_predictions = len(all_game_results) - correct_predictions
        
        print(f"\nPrediction Summary:")
        print(f"  Correct Predictions: {correct_predictions}")
        print(f"  Incorrect Predictions: {incorrect_predictions}")
        print(f"  Total Predictions: {len(all_game_results)}")
    
    # Distribution of accuracy across weeks
    accuracy_bins = {
        '90-100%': sum(1 for x in results_df['accuracy'] if x >= 0.9),
        '80-90%': sum(1 for x in results_df['accuracy'] if 0.8 <= x < 0.9),
        '70-80%': sum(1 for x in results_df['accuracy'] if 0.7 <= x < 0.8),
        '60-70%': sum(1 for x in results_df['accuracy'] if 0.6 <= x < 0.7),
        '50-60%': sum(1 for x in results_df['accuracy'] if 0.5 <= x < 0.6),
        '<50%': sum(1 for x in results_df['accuracy'] if x < 0.5),
    }
    
    print(f"\nAccuracy Distribution Across Weeks:")
    for bin_name, count in accuracy_bins.items():
        if count > 0:
            print(f"  {bin_name}: {count} week(s)")
    
    print("\n" + "="*70)
    print("TEST COMPLETE")
    print("="*70)
    
    # Return results for potential further analysis
    return {
        'weekly_results': results_df,
        'overall_accuracy': overall_accuracy,
        'overall_spread_error': overall_spread_error,
        'overall_score_error': overall_score_error,
        'total_games': total_games,
        'game_results': all_game_results
    }


def save_results_to_csv(results: Dict):
    """Save test results to CSV files."""
    if results is None:
        print("No results to save.")
        return
    
    # Save weekly results
    weekly_df = results['weekly_results']
    weekly_df.to_csv('results/monte_carlo_2024_weekly_results.csv', index=False)
    print(f"\n✓ Weekly results saved to 'monte_carlo_2024_weekly_results.csv'")
    
    # Save individual game results
    if results['game_results']:
        games_df = pd.DataFrame(results['game_results'])
        games_df.to_csv('results/monte_carlo_2024_game_results.csv', index=False)
        print(f"✓ Game-by-game results saved to 'monte_carlo_2024_game_results.csv'")
    
    # Save summary statistics
    summary = {
        'metric': [
            'Total Games',
            'Overall Accuracy',
            'Overall Spread Error',
            'Overall Score Error',
            'Best Week Accuracy',
            'Worst Week Accuracy',
            'Mean Weekly Accuracy',
            'Median Weekly Accuracy',
            'Std Dev Weekly Accuracy'
        ],
        'value': [
            results['total_games'],
            f"{results['overall_accuracy']:.1%}",
            f"{results['overall_spread_error']:.2f}",
            f"{results['overall_score_error']:.2f}",
            f"{weekly_df['accuracy'].max():.1%}",
            f"{weekly_df['accuracy'].min():.1%}",
            f"{weekly_df['accuracy'].mean():.1%}",
            f"{weekly_df['accuracy'].median():.1%}",
            f"{weekly_df['accuracy'].std():.1%}"
        ]
    }
    summary_df = pd.DataFrame(summary)
    summary_df.to_csv('results/monte_carlo_2024_summary.csv', index=False)
    print(f"✓ Summary statistics saved to 'monte_carlo_2024_summary.csv'")


def main():
    """Run the test and save results."""
    print("\n" + "🏈" * 35)
    print("MONTE CARLO MODEL - 2024 SEASON EVALUATION")
    print("🏈" * 35)
    
    # Run the test
    results = test_2024_weeks_6_18()
    
    # Save results to CSV
    if results:
        print("\n" + "="*70)
        print("SAVING RESULTS")
        print("="*70)
        save_results_to_csv(results)
    
    print("\n" + "="*70)
    print("ALL DONE!")
    print("="*70 + "\n")


if __name__ == "__main__":
    main()

