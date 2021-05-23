/* eslint no-param-reassign: 0 */
import http from '../../common/http';

const state = {
  authorized: false,
  token: null,
  nickname: null,
  sessionRefreshInterval: null,
};

const getters = {
  isAuthorized: (curState) => curState.authorized,
  token: (curState) => curState.token,
  nickname: (curState) => curState.nickname,
};

const mutations = {
  setAuthorized(curState, payload) {
    curState.authorized = true;
    curState.nickname = payload.nickname;
    curState.token = payload.token;
    curState.sessionRefreshInterval = payload.sessionRefreshInterval;
  },
  resetAuthorized(curState) {
    curState.authorized = false;
    curState.nickname = null;
    curState.token = null;
    window.clearInterval(curState.sessionRefreshInterval);
    curState.sessionRefreshInterval = null;
  },
};

const actions = {
  authorize(context, payload) {
    return new Promise((resolve, reject) => {
      http.post('authorize', {
        data: {
          email: payload.email,
          password: payload.password,
        },
      })
        .then((output) => {
          context.commit('setAuthorized', {
            nickname: output.data.nickname,
            token: output.data.token,
            sessionRefreshInterval: window.setInterval(() => {
              context.dispatch('extendSession')
                .catch((err) => {
                  context.commit('resetAuthorized');
                  reject(err);
                });
            }, (output.data.durationMinutes - 1) * 60000 - 30000),
          });
          resolve();
        })
        .catch((err) => {
          reject(err);
        });
    });
  },
  extendSession(context) {
    return new Promise((resolve, reject) => {
      http.post('extend-session', {}, {
        headers: {
          Authorization: `Bearer ${context.state.token}`,
        },
      })
        .then(() => resolve())
        .catch((err) => {
          reject(err);
        });
    });
  },
  register(context, payload) {
    return new Promise((resolve, reject) => {
      http.post('register', {
        data: {
          email: payload.email,
          nickname: payload.nickname,
          password: payload.password,
        },
      })
        .then(() => {
          resolve();
        })
        .catch((err) => {
          reject(err);
        });
    });
  },
  closeSession(context) {
    http.post('close-session', null, {
      headers: {
        Authorization: `Bearer ${context.state.token}`,
      },
    })
      .then(() => {
        console.log('logout');
        context.commit('resetAuthorized');
      })
      .catch((err) => {
        console.error(err);
      });
  },
};

export default {
  namespaced: true,
  state,
  getters,
  actions,
  mutations,
};
