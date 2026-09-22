{-# LANGUAGE CPP #-}
module CppDirectives where

#include "MachDeps.h"

#ifdef WINDOWS
pathSep :: Char
pathSep = '\\'
#else
pathSep :: Char
pathSep = '/'
#endif

#if MIN_VERSION_base(4,10,0)
newer :: Bool
newer = True
#elif defined(OLD)
newer = False
#else
newer = False
#endif

main :: IO ()
main = print pathSep
