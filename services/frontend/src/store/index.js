import Vue from 'vue';
import Vuex from 'vuex';
import auth from './modules/auth';
import document from './modules/document';

Vue.use(Vuex);

export default new Vuex.Store({
  state: {
    treeMenuData: {
      root: {
        titolo: 'Caricamento dal server in corso...',
      },
    },
  },
  getters: {
    treeMenuData: (state) => state.treeMenuData,
  },
  mutations: {
    setTreeMenuData(state, newValue) {
      state.treeMenuData = newValue;
    },
  },
  modules: {
    auth,
    document,
  },
});
