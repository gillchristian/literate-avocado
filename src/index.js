import {Elm} from './Main.elm';
import * as serviceWorker from './serviceWorker';

const KEY = 'persisted_config';

const app = Elm.Main.init({
  node: document.getElementById('root'),
});

app.ports.saveToStorage.subscribe((data) => {
  localStorage.setItem(KEY, JSON.stringify(data));
});

app.ports.doLoadFromStorage.subscribe(() => {
  let data = localStorage.getItem(KEY);
  if (data) {
    data = JSON.parse(data);
  }
  app.ports.loadFromStorage.send(data);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
