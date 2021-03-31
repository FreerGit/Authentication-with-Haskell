import { h } from 'preact';
import { Router } from 'preact-router';

// Code-splitting is automated for `routes` directory
import Header from './header/header';
import Home from '../routes/home/home';
import Register from '../routes/register/register';
import Login from '../routes/login/login';
import AuthenticatedRoutes from '../routes/AuthenticatedRoutes/authenticatedRoutes';

import { logoutAllTabs } from '../lib/auth';

const App = () => {
	window.addEventListener('storage', logoutAllTabs);
	return (
		<div id="app" >
			<Header />
			<Router>
				<AuthenticatedRoutes path="/authed/:rest*" />
				<Home path="/" />
				<Register path="/register" />
				<Login path="/login" />
				<Home default />
				{/* <Error type="404" default /> */}
			</Router>
		</div >
	);
};

export default App;
