import { h } from 'preact';
import style from './style.css';
import { useState } from 'preact/hooks';

const Register = () => {
	const [name, setName] = useState();
	const [password, setPassword] = useState();

	const handleSubmit = (event) => {
		event.preventDefault();
		console.log(`${name} ${password}`);
	};

	return (
		<div class={style.register}>
			<h1>Register</h1>
			<p>This is the Register component.</p>
			<form onSubmit={handleSubmit}>
				<input type="text" onInput={(c) => setName(c.target.value)} />
				<input type="text" onInput={(c) => setPassword(c.target.value)} />
				<button type="submit">Register Now!</button>
			</form>
		</div >
	);

};

export default Register;




