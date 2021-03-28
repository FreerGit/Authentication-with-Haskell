import { h } from 'preact';
import style from './style.css';
import { useEffect } from 'preact/hooks';

import { logout } from '../../lib/auth'


const Hidden = () => {
	// useEffect(() => {
	// 	inMem
	// });

	return (
		<div class={style.hidden}>
			<h1>Hidden comp</h1>
			<button onClick={() => { logout(); }} />

		</div >
	);

};

export default Hidden;




