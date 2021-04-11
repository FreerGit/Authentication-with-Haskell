const postData = async (urlExtender, data) => {
	const url = `${process.env.PREACT_APP_SERVER_URL}${urlExtender}`;
	const resp = await fetch(url, {
		method: 'POST',
		headers: {
			'Content-Type': 'application/json',
		},
		body: JSON.stringify(data),
	});
	const json = await resp.json();
	return resp.ok ? json : Promise.reject(json);
};

const safePostData = async (urlExtender, data) => {
	const url = `${process.env.PREACT_APP_SERVER_URL}${urlExtender}`;
	const resp = await fetch(url, {
		method: 'POST',
		credentials: 'include',
		headers: {
			'Content-Type': 'application/json',
			'Cache-Control': 'no-cache'
		},
		body: JSON.stringify(data),
	});
	const json = await resp.json();
	return resp.ok ? json : Promise.reject(json);
};

export { postData, safePostData };