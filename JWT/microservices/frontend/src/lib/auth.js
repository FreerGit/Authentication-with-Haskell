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
		route('/authed/hidden', true);
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

const fetchNewJWT = async () => {
	console.log('fetching new JWT');
	const url = `${process.env.PREACT_APP_SERVER_URL}refreshToken`;
	const resp = await fetch(url, {
		method: 'POST',
		credentials: 'include',
	});
	if (resp.ok) {
		const json = await resp.json();
		globalMemoryToken = json;
		console.log('ok');
		return true;
	}
	console.log('logging out');
	return false;
};

const isAuthenticated = async () => {
	const token = globalMemoryToken;
	if (token) {
		return true;
	}
	const validRefreshToken = fetchNewJWT();
	if (validRefreshToken) {
		return true;
	}
	return false;
};

const tokenCheckMiddleware = async (request) => {
	if (globalMemoryToken) {
		request['Authorization'] = `Bearer: ${globalMemoryToken}`;
	}
};

export { login, logout, logoutAllTabs, fetchNewJWT, isAuthenticated };