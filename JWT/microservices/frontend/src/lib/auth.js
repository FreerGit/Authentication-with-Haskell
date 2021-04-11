import { route } from 'preact-router';
import decode from 'jwt-decode';
let globalMemoryToken;

const login = async ({ jwtToken }, redirect = false) => {
	const decodedJWT = await decode(jwtToken);
	globalMemoryToken = {
		jwt: jwtToken,
		exp: decodedJWT.exp
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
	const url = `${process.env.PREACT_APP_SERVER_URL}refreshToken`;
	const resp = await fetch(url, {
		method: 'POST',
		credentials: 'include',
	});
	if (resp.ok) {
		const json = await resp.json();
		console.log(json);
		globalMemoryToken = json;
		console.log('yuuup');
		return true;
	}
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

const getJWTInfo = async () => {
	return globalMemoryToken;
};

export { login, logout, logoutAllTabs, fetchNewJWT, isAuthenticated, getJWTInfo };