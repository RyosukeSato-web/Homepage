# Haskell

## Install
`Haskell` can be installed by `brew` command. First, you install `stack` as follows:
```
brew install stack
```
Next, using stack, you install `ghc`, which is a compiler of Haskell, and basic packages:
```
stack setup
```
Then you can use Haskell. If you want to use Haskell in visual studio code, then you need to follow the steps below:
```
brew install llvm@9
```
In addition, you install an extension of vs code called "Haskell". Then you can use Haskell in vs code. 

## Trial
As a test, let us create the file `hello.hs` and write the following code:
```haskell
main :: IO ()
main = putStrLn "hello"
```
To compile and run this code, you follow the steps below:
```
ghc -o hello hello.hs
./hello
```
Upon completing the steps up to now, you will get 
```
hello
```

or the code can run as follows:
```
stack runghc ***.hs
```