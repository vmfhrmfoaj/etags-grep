Feature: Choose among multiple tags for the same name
  In order to do choose among multiple tags for the same name
  As a developer
  I want to do show the multiple tags like grep-mode

  Scenario: Find a single tag in specific TAGS file
    Given I have "features/support/for-test/a.TAGS" tags table
     When I find a tag that named "a"
     Then I should place at a line "3" of "a.c"

  Scenario: Find a multiple tag in specific TAGS file
    Given I have "features/support/for-test/aa.TAGS" tags table
     When I find a tag that named "a"
     Then I should show new window
      And I switch the focus to the window that having a "grep-mode" buffer
      And I should show following text
      """
      ./aa.cc:3:int a(
      ./aa.cc:8:int a(
      """

  Scenario: Find a multiple tag in multiple TAGS file
    Given I have "features/support/for-test/aa.TAGS" tags table
      And I have "features/support/for-test/a.TAGS" tags table
     When I find a tag that named "a"
     Then I should show new window
      And I switch the focus to the window that having a "grep-mode" buffer
      And I should show following text
      """
      ./a.c:3:int a(
      ./aa.cc:3:int a(
      ./aa.cc:8:int a(
      """

  Scenario: Move a tag
    Given I have "features/support/for-test/aa.TAGS" tags table
      And I have "features/support/for-test/a.TAGS" tags table
     When I find a tag that named "a"
     When I move the cursor to "./a.c:3:int a("
      And I press "C-m" key
     Then I should place at a line "3" of "a.c"
     When I switch the focus to the window that having a "grep-mode" buffer
      And I move the cursor to "./aa.cc:3:int a("
      And I press "C-m" key
     Then I should place at a line "3" of "aa.cc"
     When I switch the focus to the window that having a "grep-mode" buffer
      And I move the cursor to "./aa.cc:8:int a("
      And I press "C-m" key
     Then I should place at a line "8" of "aa.cc"

  Scenario: Move next/previous a tag
    Given I have "features/support/for-test/aa.TAGS" tags table
      And I have "features/support/for-test/a.TAGS" tags table
      And I find a tag that named "a"
     When I press "n" key
     Then I should place a buffer that named "*etags-grep*"
      And I should show "./a.c:3:int a(" in current line
      And I switch the other window
      And I should place at a line "3" of "a.c"
      And I switch the other window
     When I press "n" key
     Then I should place a buffer that named "*etags-grep*"
      And I should show "./aa.cc:3:int a(" in current line
      And I switch the other window
      And I should place at a line "3" of "aa.cc"
      And I switch the other window
     When I press "n" key
     Then I should place a buffer that named "*etags-grep*"
      And I should show "./aa.cc:8:int a(" in current line
      And I switch the other window
      And I should place at a line "8" of "aa.cc"
      And I switch the other window
     When I press "p" key
     Then I should place a buffer that named "*etags-grep*"
      And I should show "./aa.cc:3:int a(" in current line
      And I switch the other window
      And I should place at a line "3" of "aa.cc"
      And I switch the other window
     When I press "p" key
     Then I should place a buffer that named "*etags-grep*"
      And I should show "./a.c:3:int a(" in current line
      And I switch the other window
      And I should place at a line "3" of "a.c"
      And I switch the other window
