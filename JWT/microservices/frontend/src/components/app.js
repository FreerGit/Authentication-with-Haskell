import { h } from 'preact';
import { route, Router } from 'preact-router';

import Header from './header/header';

// Code-splitting is automated for `routes` directory
import Home from '../routes/home/home';
import Register from '../routes/register/register';
import Login from '../routes/login/login';
import Hidden from '../routes/hidden/hidden';
import { logoutAllTabs, isAuthenticated } from '../lib/auth';

const AuthenticatedRoutes = () => {
	const handleAuthRoutes = async () => {
		const isAuthed = await isAuthenticated();
		console.log(isAuthed);
		if (!isAuthed) route('/login', true);
	};
	return (
		<div>
			<Router onChange={handleAuthRoutes}>
				<Hidden path={'/hidden'} />
			</Router>
		</div>
	);
};


const App = () => {
	window.addEventListener('storage', logoutAllTabs);
	// fetchNewJWT();


	return (
		<div id="app" >
			<Header />
			<Router>
				<Home path="/" />
				<Register path="/register" />
				<Login path="/login" />
				<AuthenticatedRoutes path="/authed/:rest*" />
				<Home default />
				{/* <Error type="404" default /> */}
			</Router>
		</div >
	);
};

export default App;
