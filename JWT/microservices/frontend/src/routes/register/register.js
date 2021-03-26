import { h } from 'preact';
import style from './style.css';
import { useState } from 'preact/hooks';
import postData from '../../lib/requests';


const Register = () => {
	const [name, setName] = useState();
	const [password, setPassword] = useState();

	const handleSubmit = (event) => {
		const registerInfo = {
			email: name,
			password
		};
		event.preventDefault();
		postData('register', registerInfo)
			.then(data => {
				console.log(data);
			})
			.catch(error => {
				console.log(error);
			});
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




