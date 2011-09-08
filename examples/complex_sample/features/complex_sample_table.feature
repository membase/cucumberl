Feature: Multiple Operations
  In order to avoid silly mistakes
  As a math idiot
  I want a calculator to do multiple operations

  Scenario Outline:
    Given I have cleared the calculator
    And I have entered <a> into the calculator
    And I have entered <b> into the calculator
    When I press <op>
    Then the result should be <ab> on the screen
    Examples:
      |  a | b | ab | op       |
      |  1 | 1 | 2  | add      |
      |  1 | 3 | 3  | multiply |
      |  2 | 3 | 6  | multiply |
      | 10 | 1 | 11 | add      |

  Scenario: Add two numbers
    Given I have cleared the calculator
    And I have entered 5 into the calculator
    And I have entered 7 into the calculator
    When I press add
    Then the result should be 12 on the screen
