# Monad Lisa
Dhriti Kishore (dhriti) and Kelly Tan (kellytan)


## Files (within monad-lisa):
    app/Main.hs                     -- Main file

    data/blurFlower2.jpg            -- Sample image files
    data/flower.jpg
    data/flower2.jpg
    data/graduation.jpg
    data/koen.jpg
    data/mona_lisa.jpg
    data/Monkey.jpg

    extra/benchmark.xlsx            -- Benchmarking data (complete with graph!)

    kd/test.kd                      -- Sample kd file (currently the benchmark test)
    kd/monad_lisa.kd                -- Mona Lisa sample
    kd/zombie_lisa.kd               -- Mona Lisa's zombie cousin

    src/Evaluator.hs                -- Evaluator for IT (Image Transformations)
    src/ImageParser.hs              -- Parser for kd files
    src/ImageTransformation.hs      -- Image transformation functions
    src/Parser.hs                   -- Parser from class
    src/ParserCombinators.hs        -- Parser combinator from class (w/modifications)

    test/Spec.hs                    -- Tests for image transformation functions,
                                       evaluator, and parser.

    monad-list.cabal                -- Cabal file for haskell stack that contains
                                       build instructions and dependencies


## Dependencies (can just run stack build):
    base >= 4.7 && < 5
    JuicyPixels
    repa
    JuicyPixels-repa
    HUnit
    QuickCheck
    containers
    repa-algorithms

    llvm    -- not cabal
    clang   -- not cabal


## Main Components:
    Image Parser                        -- Parser for kd files
    Image Transformation                -- Image transformation functions
    Evaluator                           -- Evaluator for IT (Image Transformations)


## Compile:
stack build


## Run executable:
stack exec monad-lisa-exe


## Run tests:
stack test


## Benchmarking:
time ./.stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/monad-lisa-exe/monad-lisa-exe +RTS -N<# of threads>
