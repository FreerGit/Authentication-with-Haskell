const postData = async (urlExtender, data) => {
	const resp = await fetch(`${process.env.PREACT_APP_BASE_URL}${urlExtender}`, {
		method: 'POST',
		headers: {
			'Content-Type': 'application/json',
		},
		body: JSON.stringify(data),
	}).then(body => body.json())
		.then(data => data);
	return resp;
};

export default postData;