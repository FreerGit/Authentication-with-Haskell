import { h } from 'preact';
import { Router } from 'preact-router';

import Header from './header/header';

// Code-splitting is automated for `routes` directory
import Home from '../routes/home/home';
import Register from '../routes/register/register';
import Login from '../routes/login/login';

const App = () => (
	<div id="app">
		<Header />
		<Router>
			<Home path="/" />
			<Register path="/register" />
			<Login path="/login" />
		</Router>
	</div>
);

export default App;
