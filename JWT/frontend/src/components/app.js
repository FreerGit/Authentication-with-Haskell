import { h } from 'preact';
import { Router } from 'preact-router';

import Header from './header/header';

// Code-splitting is automated for `routes` directory
import Register from '../routes/register/register';

const App = () => (
    <div id="app">
        <Header />
        <Router>
            <Register path="/" />
        </Router>
    </div>
)

export default App;
