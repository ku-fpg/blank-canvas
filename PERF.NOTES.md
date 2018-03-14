This branch is meant to allow us to see the performance measurements between the weak, strong
and applicative bundling strategies of the remote monad. 

The hypothesis is that the weak bundling will cause the examples to crawl.
The strong will be noticeably quicker on most of the examples but should slow down in the
IsPointInPath, MeasureText and ToDataURL examples

These examples should be much faster once the applicative bundling is in place.


In an earlier version of blank canvas (which used the Command and Procedure parameterized remote
monad) was having weird results with the ToDataURL example. For some reason it was able to 
perform very quickly with simply the strong bundling. We will need to look into the reasoning
behind that.
