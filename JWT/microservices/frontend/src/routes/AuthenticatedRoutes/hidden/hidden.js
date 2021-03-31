import { h } from 'preact';
import style from './style.css';
import { useEffect } from 'preact/hooks';
import { logout, fetchNewJWT } from '../../../lib/auth';


const Hidden = () => {
	//before JWT expires, fetch new one with httpOnly refreshToken
	useEffect(() => {
		const fetchNewBeforeExpiry =
			(process.env.PREACT_APP_TOKEN_TIMER_MIN
				- process.env.PREACT_APP_LEEWAY_FOR_TOKEN_TIMER_MIN) * (6 * 1000); // minutes -> milliseconds
		const interval = setInterval(() => fetchNewJWT(), fetchNewBeforeExpiry);

		return () => clearInterval(interval);
	}, []);

	return (
		<div class={style.hidden}>
			<h1>Hidden comp</h1>
			<button onClick={() => { logout(); }} />

		</div >
	);

};

export default Hidden;




