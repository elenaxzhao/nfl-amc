"""
Command Line NFL Predictor
Usage: python cli_predictor.py KC BUF 8 --home-injuries 1 --away-injuries 4 --wind 10
"""

from .nfl_bayesian_network import NFLBayesianNetwork, GameFeatures
import argparse

def predict_game_cli(home_team, away_team, week, home_injuries=0, away_injuries=0, 
                    wind=5, temp=70, divisional=False, home_rest=7, away_rest=7):
    """Predict a game using command line arguments"""
    
    # Load model
    print("Loading Bayesian Network model...")
    try:
        bn = NFLBayesianNetwork()
        bn.load_model('nfl_bayesian_model.pkl')
        print("✓ Model loaded successfully!")
    except FileNotFoundError:
        print("Model not found. Training new model...")
        bn = NFLBayesianNetwork()
        bn.train()
        bn.save_model('nfl_bayesian_model.pkl')
        print("✓ Model trained and saved!")
    
    # Create features
    features = GameFeatures(
        home_key_injuries=home_injuries,
        away_key_injuries=away_injuries,
        home_total_injuries=home_injuries * 2,
        away_total_injuries=away_injuries * 2,
        wind_speed=wind,
        temperature=temp,
        week=week,
        is_divisional=divisional,
        home_rest_days=home_rest,
        away_rest_days=away_rest
    )
    
    # Make prediction
    prediction = bn.predict(features)
    
    # Display results
    print(f"\n" + "="*60)
    print("NFL GAME PREDICTION")
    print("="*60)
    print(f"{away_team} @ {home_team} (Week {week})")
    print(f"")
    print(f"🏠 Home Team Win Probability: {prediction.home_win_probability:.1%}")
    print(f"✈️  Away Team Win Probability: {prediction.away_win_probability:.1%}")
    print(f"")
    print(f"Confidence Level: {prediction.confidence.upper()}")
    print(f"")
    
    if prediction.key_factors:
        print("Key Factors:")
        for i, factor in enumerate(prediction.key_factors, 1):
            print(f"  {i}. {factor}")
        print("")
    
    print("💰 Betting Recommendation:")
    print(f"   {prediction.betting_recommendation}")
    print("="*60)
    
    return prediction

def main():
    parser = argparse.ArgumentParser(description='NFL Game Predictor')
    parser.add_argument('home_team', help='Home team abbreviation (e.g., KC)')
    parser.add_argument('away_team', help='Away team abbreviation (e.g., BUF)')
    parser.add_argument('week', type=int, help='Week number')
    parser.add_argument('--home-injuries', type=int, default=0, help='Home team key injuries')
    parser.add_argument('--away-injuries', type=int, default=0, help='Away team key injuries')
    parser.add_argument('--wind', type=float, default=5, help='Wind speed (mph)')
    parser.add_argument('--temp', type=float, default=70, help='Temperature (F)')
    parser.add_argument('--divisional', action='store_true', help='Is divisional game')
    parser.add_argument('--home-rest', type=int, default=7, help='Home team rest days')
    parser.add_argument('--away-rest', type=int, default=7, help='Away team rest days')
    
    args = parser.parse_args()
    
    predict_game_cli(
        home_team=args.home_team.upper(),
        away_team=args.away_team.upper(),
        week=args.week,
        home_injuries=args.home_injuries,
        away_injuries=args.away_injuries,
        wind=args.wind,
        temp=args.temp,
        divisional=args.divisional,
        home_rest=args.home_rest,
        away_rest=args.away_rest
    )

if __name__ == "__main__":
    main()
