Feature: Reporting Failures
  In order to make silly mistakes visible
  As a cucumberl user
  I want to be told about failing steps

  Scenario: Fail a given step
    Given a step that works
    When I come across a failing step
    Then cucumberl should show this as a failure
