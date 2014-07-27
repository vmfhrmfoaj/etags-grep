Feature: Choose among multiple tags for the same name
  In order to do choose among multiple tags for the same name
  As a developer
  I want to do show the multiple tags like grep-mode

  Scenario: Find a tag with multiple directories
    Given I have "features/support/for-test-2/aa/aa.TAGS" tags table
      And I have "features/support/for-test-2/a/a.TAGS" tags table
     When I find a tag that named "a"
     Then I should show new window
      And I switch the focus to the window that having a "grep-mode" buffer
      And I should show following text
      """
      ./a/a.c:3:int a(
      ./aa/aa.cc:3:int a(
      ./aa/aa.cc:8:int a(
      """

  Scenario: Move a tag with multiple directories
    Given I have "features/support/for-test-2/aa/aa.TAGS" tags table
      And I have "features/support/for-test-2/a/a.TAGS" tags table
     When I find a tag that named "a"
     When I move the cursor to "./a/a.c:3:int a("
      And I press "C-m" key
     Then I should place at a line "3" of "a.c"
     When I switch the focus to the window that having a "grep-mode" buffer
      And I move the cursor to "./aa/aa.cc:3:int a("
      And I press "C-m" key
     Then I should place at a line "3" of "aa.cc"
     When I switch the focus to the window that having a "grep-mode" buffer
      And I move the cursor to "./aa/aa.cc:8:int a("
      And I press "C-m" key
     Then I should place at a line "8" of "aa.cc"
