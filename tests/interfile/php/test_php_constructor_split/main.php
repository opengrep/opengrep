<?php

function main() {
    $taintedInput = source();
    $user = new User($taintedInput);
    $result = $user->getProfile();
    
    // Test field assignment taint flow
    $taintedInput2 = source();
    $fieldUser = new FieldUser();
    $fieldUser->name = $taintedInput2;
    $fieldResult = $fieldUser->getProfile();
    
    return $result;
}
