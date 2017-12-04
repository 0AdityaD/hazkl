{
module Parser where
import Token
}

%name parse
%tokentype {Token}
%error  {parseError}
