# MATCH: no other elements
%{some_item: 0}

# MATCH: 1 other element
%{some_item: 0, some_other_item: 1}

# MATCH: 1 other element, before the target
%{some_other_item: 1, some_item: 0}

# MATCH: 2 other elements
%{some_item: 0, some_other_item: 1, yet_another_other_item: 2}

# MATCH: 2 other elements
%{some_other_item: 1, some_item: 0, yet_another_other_item: 2}

# no match: arrow syntax, not keyword
%{"some_item" => 0}

# no match: arrow syntax with other elements
%{"some_item" => 0, "some_other_item" => 1}
