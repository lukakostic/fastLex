# fastLex
A very small *(single file)* simple but very flexible lexer

![image](https://github.com/lukakostic/fastLex/assets/41348897/452adcea-6a50-4f11-b382-e3ac663242e7)

Some included operators to demonstrate the lexer:

![image](https://github.com/lukakostic/fastLex/assets/41348897/78990d99-d4f8-48a0-aad2-231343b2d8a7)

## TODO

- Parallelize lexing

## Unsupported

- Number literals starting or ending with a dot: `.123` or  `1.` just do `0.123` or `1.0` instead.
   - will probably be added
