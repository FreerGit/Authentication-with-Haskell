import { route } from 'preact-router';

let globalMemoryToken;

const login = async ({ jwtToken, jwtTokenExp }, redirect = false) => {
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
	const url = `${process.env.PREACT_APP_SERVER_URL}refreshToken`;
	const resp = await fetch(url, {
		method: 'POST',
		credentials: 'include',
	});
	if (resp.ok) {
		const json = await resp.json();
		globalMemoryToken = json;
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

export { login, logout, logoutAllTabs, fetchNewJWT, isAuthenticated };