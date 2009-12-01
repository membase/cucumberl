Feature: Multiplication
  In order to avoid silly mistakes
  As a math idiot
  I want to be told the product of two numbers

  Scenario: Multiply two numbers
    Given I have entered 50 into the calculator
    And I have entered 70 into the calculator
    When I press multiply
    Then the result should be 3500 on the screen
