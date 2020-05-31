%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(geometry).  
-export([area/1]). 

area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radious})          -> 3.14159 * Radious * Radious;
area({hypot, X, Y})              -> math:sqrt(X*X + Y*Y);
area({square, Side})             -> Side * Side.
