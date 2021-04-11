import { h } from 'preact';
import { Router, route } from 'preact-router';
import Hidden from './hidden/hidden';

import { isAuthenticated } from '../../lib/auth';


const AuthenticatedRoutes = () => {
	const handleAuthRoutes = async () => {
		const isAuthed = await isAuthenticated();
		if (!isAuthed) route('/login', true);
	};
	return (
		<div>
			<Router onChange={handleAuthRoutes}>
				<Hidden path={'/authed/hidden'} default />
			</Router>
		</div>
	);
};

export default AuthenticatedRoutes;