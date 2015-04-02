# Receipt Generator

## Description

GUI receipt/quote generator tool for business. Outputs HTML file with formatting as receipt/quote.

## Compile (Windows)

```
raco exe -o receipt_generator.exe receipt_generator.rkt
raco distribute receipt_generator receipt_generator.exe
del /f receipt_generator.exe
```

## Compile (Mac OS X)

```
raco exe -o receipt_generator receipt_generator.rkt
raco distribute receipt_generator_dir receipt_generator
rm -f receipt_generator
mv -f receipt_generator_dir receipt_generator
```