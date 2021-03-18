# Authentication with Haskell

## Tech-stack
Haskell with servant as backend

Elm frontend 

## Purpose
Make a simple website with authentication, from scratch or from a library is undecided. I will have to see what
haskell libraries exist currently.

Simply to learn more about FP and authentication.


## Databases
postgresql for the long term storage of users however redis will be used to store logged in users session.
That way I can leverage the speed of RAM storage when checking if a users has authentication. Thats should (haven't implemented it yet) speed up the request by a order of magnitude. I hope redis will be a good fit since it's purpose is to store key-value type of data. A perfect fit for users ID as key and session/bool as value (Is user logged in yes/no). Also caching certain requests may be useful, not sure how/which yet though.

I`m not sure how long i should cache some requests, currently most requests are cached for 10 minutes in the redis database. Different requests should have different cache expirary times since the correctness of the call matters. 

### Findings
* Caching common requests such as fetch user with userID decreased the request time from an average of 15ms to 3.5ms on localhost. Thats one helluva speed increase!

* Surprisingly great libraries for queries, both for postgres and redis. Haskell is may more mature for backend development than i initially thought.

* The lack of tutorials and general information for haskell development is made up by great documentation.

## non obvious TODOs
### Security
As of now i naively handle users with an integer ID, that means GET requests such as /users/2 works... obvious brute force attack to guess user IDs with a counter.

### Cold requests
The connection with postgres will go cold after a while until the backend reconnects to it, the first request will take 200ms. Not catastrophic but not nice, realistic to keep connection at all times? Not sure how to fix that atm.
