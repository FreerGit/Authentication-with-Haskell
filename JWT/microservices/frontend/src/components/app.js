import { h } from 'preact';
import { Router } from 'preact-router';

import Header from './header/header';

// Code-splitting is automated for `routes` directory
import Home from '../routes/home/home';
import Register from '../routes/register/register';
import Login from '../routes/login/login';
import Hidden from '../routes/hidden/hidden';

const App = () => {
	return (
		<div id="app" >
			<Header />
			<Router>
				<Home path="/" />
				<Register path="/register" />
				<Login path="/login" />
				<Hidden path="/hidden" />
			</Router>
		</div >
	);
};

export default App;
