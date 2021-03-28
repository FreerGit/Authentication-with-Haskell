import { h } from 'preact';
import style from './style.css';
import { useEffect } from 'preact/hooks';


const Hidden = () => {
	useEffect(() => {

	});

	return (
		<div class={style.hidden}>
			<h1>Hidden comp</h1>

		</div >
	);

};

export default Hidden;




