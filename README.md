# fastLex
A very small *(single file)* simple but very flexible lexer

![image](https://github.com/lukakostic/fastLex/assets/41348897/452adcea-6a50-4f11-b382-e3ac663242e7)

Some included operators to demonstrate the lexer:

![image](https://github.com/lukakostic/fastLex/assets/41348897/78990d99-d4f8-48a0-aad2-231343b2d8a7)

## How to use
I build with g++ -std=c++20  but im sure it works for any.
```c++
#include "fastLex.cpp"

fastLex lex;
lex.__populateCppTokens(); //for example
lex.init();
   
lex.parse( (char*) "some code!" );
    
//Example of getting parsed tokens out
for(auto s : lex.parsed){
   bool ident = true;
   if(s.type==Parsed::Operator){ ident=false; cout<<"op\t"; }
   else if(s.type==Parsed::Identifier) cout<<"ident\t";
   else if(s.type==Parsed::Number) cout<<"number\t";
   else if(s.type==Parsed::String) cout<<"string"<<("'\0\"\0`\0```"[s.info.stringType*2])<<"\t";
   
   if(ident) cout << " ["<<lex.idents[s.idx]<<"]\n";
   else cout << " ["<<lex.all[s.idx].str<<"]\n";
}

lex.free();
```

## TODO

- Parallelize lexing

## Unsupported

- Number literals starting or ending with a dot: `.123` or  `1.` just do `0.123` or `1.0` instead.
   - will probably be added
