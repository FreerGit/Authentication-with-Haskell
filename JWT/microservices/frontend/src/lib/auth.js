import { route } from 'preact-router';

let globalMemoryToken;

const login = async ({ jwtToken, jwtTokenExp }, redirect = false) => {
	console.log(jwtToken);
	console.log(jwtTokenExp);
	globalMemoryToken = {
		token: jwtToken,
		expiry: jwtTokenExp
	};

	if (!redirect) {
		route('/hidden', true);
	}
	return;
};

export { login };