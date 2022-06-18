{
module Parser.Lexer where
}

%wrapper "basic"

$alpha    = [A-Z]
$spaces   = [\ \t]

tokens :-
       $white+ 			          ;
       $alpha[$alpha 0-9 \' \’]*  { \x -> TOKEN_VARIABLE x   }
       \(		                  { \x -> TOKEN_OPENED_BRACE }
       \) 		                  { \x -> TOKEN_CLOSED_BRACE }
       \&		                  { \x -> TOKEN_AND 	     }
       \!		                  { \x -> TOKEN_NOT	         }
       \|		                  { \x -> TOKEN_OR	         }
       "->"		                  { \x -> TOKEN_IMPLICATION  }

{
data Token
     = TOKEN_VARIABLE String
     | TOKEN_OPENED_BRACE
     | TOKEN_CLOSED_BRACE
     | TOKEN_AND
     | TOKEN_NOT
     | TOKEN_OR
     | TOKEN_IMPLICATION
}
