# authie
For generation of a safe JWT_secret, simply run:
```javascript
node -e "console.log(require('crypto').randomBytes(256).toString('base64'));"
```