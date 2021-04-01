# JWT Authentication
## The basics
JWT or 'jot' are basically a way of doing authentication in stateless way, which means that in theory when a users requests a resource or wants to login, no database lookups are needed. This would massively decrease authentication time since the server is not IO bound. For an in depth explanation of what the JWT standard is: https://jwt.io/introduction

There are many ways to implement authentication with JWTs, to be fair, after implementing a authentication system with JWT, I am fairly convinced it's a bad choice for most use cases. Client-to-Server authentication is certainly not the best use case for JWT. The problem is that there are just too many security flaws when a potentially malicious user can see and interact with a JWT. Notably any XSS exploits in your website will cause real issues, remember the JWT is stateless, so if someone gets a hold of someone's JWT they can use it until it expires. We will look at ways to mitigate this though.

## How does this implementation work?
**Lets start with registration:**
1. User types in username and password in the client.
2. The client sends that data in a POST request to the server, make sure to always use HTTPS.
3. Server stores username/email with a hashed and salted version of the password into the database. This is done with Bcrypt encryption with a private key, keep this key safe!
4. The server sends an OK to the client.
5. The client reroutes the user to the login page.

**Login flow**

1. User types in username and password in the client.
2. The client sends that data in a POST request to the server, make sure to always use HTTPS.
3. The server will compare the username and password pair with the username and hashed password pair in the database. If these are the same, then we have a legit user.
4. The server creates a JWT along side with a refresh token.
5. The client reroutes to the hidden page.
6. Start the refresh timer countdown.

**What is a refresh token?**

A refresh token is essentially a JWT embedded in an httpOnly cookie. The refresh token has a longer expiration time because the refresh token will be used as a way to maintain a longer session for the client. Users shouldn't have to login everytime the JWT expires, so the refresh token is used to refresh the JWT. For example: the JWT has an expiry of 5 minutes, just before the 5 minutes has passed on the client it will use the refresh token to get a new JWT that will last another 5 minutes. 

**The refresh token flow**

1. When the refresh timer is 0, the client calls /refreshToken. This will send the refresh token along with the call since it is a httpOnly cookie.
2. The server will check the refresh tokens validity, has it expired? (there are a lot more checks that can be done, such as issuer).
3. If the server deems that the refresh token is valid then create a new JWT and refresh token and send it off just like the login.
4. Client stores the new JWT in memory (do not store the JWT in localStorage). Then resume the countdown based on the expiry time of the JWT.

In this way we can assure that the user has a "persisted" session on the website in a completely stateless way. On this implementation the call to /refreshToken takes about 4 ms (on localhost, so no latency). This is because the call is not IO bound but also since Haskell is surprisingly fast.

## Notes on security
Some basics, JWT has to be used over HTTPS or it is instantly rendered useless. With a properly setup CORS-config and WAI middleware (the base layer of the webserver) this really is not hard to implement. The server will simply not respond to/with http.

I noted earlier that the overall security of the website has to be quite high in order for the stateless JWT to be secure. Now to be fair, we can have a stateful implementation of JWT (such as a blacklist) but at that point a session based authentication system makes more sense. The crux is that the server has no way of knowing if the JWT was sent by the client through the user or with an XSS attack (or other exploits). So if an exploit can read JS memory and grab the JWT and use it to request data, then that hacker has full authenticity for <5 minutes. Now at the very least we should use proper mechanisms to make sure that the server only responds to the client through CORS. But again, if malicious JS is injected into that client it doesn't matter.

So we have covered the very basics of client-to-server authentication with JWT, it works, it's probably OK from an security perspective but.... I wouldn't trust a bank if they had my implementation :man_shrugging:.

The more interesting use case is server-to-server authentication! I believe that a session based authentication system makes a lot more sense for client-to-server authentication but combining JWTs for server-to-server authentication may be a really good middlegournd. Because assuming that the connection between the servers are over HTTPS we do not have to be worried about ill intent, assuming the servers ONLY allow connections between eachother and will not communicate with outsiders (CORS). We can how have a stateful safe authentication system for the users and a stateless fast authentication for microservice requests. The microservice only need to verify that the issuer of the token is authentic and it has not expired. If that holds true the server will respond to the authenticated task. This would be an order of magnitude faster than verifying than having each microservice verifying sessions through database IO. 

