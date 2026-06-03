"""Ordered pipeline steps. Each step exposes a `run(config, **inputs)` function
returning a dict of artifacts/paths to hand off to the next step.
"""
