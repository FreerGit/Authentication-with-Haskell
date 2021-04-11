import { h } from 'preact';
import style from './style.css';
import { useEffect } from 'preact/hooks';
import { logout, fetchNewJWT, getJWTInfo } from '../../../lib/auth';


const Hidden = () => {
	//before JWT expires, fetch new one with httpOnly refreshToken
	useEffect(() => {
		let interval;
		getJWTInfo().then(token => {
			const expiry = new Date(token.exp * 1000);
			const timeToExpiry = expiry - Date.now();
			interval = setInterval(() => fetchNewJWT(), timeToExpiry);
		});
		return () => clearInterval(interval);
	}, []);

	return (
		<div class={style.hidden}>
			<h1>Hidden comp</h1>
			<button class={style.logout} onClick={() => { logout(); }}>Logout</button>

		</div >
	);

};

export default Hidden;




