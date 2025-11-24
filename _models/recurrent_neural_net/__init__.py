#!/usr/bin/env python3
"""
NFL Game Prediction using Recurrent Neural Networks
====================================================

A PyTorch-based sequence model for predicting NFL game outcomes.
"""

from .data_loader import NFLDataLoader, TimeAwareSplitter
from .feature_engineering import FeatureEngineer
from .sequence_builder import SequenceBuilder, RecencyWeightCalculator
from .rnn_model import NFLGamePredictor, TeamEncoder, WinPredictionHead, ScorePredictionHead
from .trainer import Trainer, TrainingConfig, CombinedLoss
from .metrics import MetricsCalculator, Calibrator, ResultsLogger

__version__ = "1.0.0"

__all__ = [
    # Data
    'NFLDataLoader',
    'TimeAwareSplitter',
    
    # Features
    'FeatureEngineer',
    
    # Sequences
    'SequenceBuilder',
    'RecencyWeightCalculator',
    
    # Model
    'NFLGamePredictor',
    'TeamEncoder',
    'WinPredictionHead',
    'ScorePredictionHead',
    
    # Training
    'Trainer',
    'TrainingConfig',
    'CombinedLoss',
    
    # Evaluation
    'MetricsCalculator',
    'Calibrator',
    'ResultsLogger',
]

