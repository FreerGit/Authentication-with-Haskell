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

const logout = async () => {
	globalMemoryToken = null;
	window.localStorage.setItem('logout', Date.now());
	route('/login', true);
};

const logoutAllTabs = async (storageEvent) => {
	if (storageEvent.key === 'logout') {
		route('/login', true);
	}
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

export { login, logout, logoutAllTabs };