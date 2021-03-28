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

const getTokenOrReroute = async () => {
	const token = globalMemoryToken;
	if (!token) {
		route('/login', true);
	}
	return token;
};

const tokenCheckMiddleware = async (request) => {
	if (globalMemoryToken) {
		request['Authorization'] = `Bearer: ${globalMemoryToken}`;
	}
};

export { login };