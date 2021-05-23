/* eslint no-param-reassign: 0 */

const state = {
  top: null,
  left: null,
  height: null,
  width: null,
  padding: null,
  margin: null,
  mobile: null,
  orientation: null,
  touchCell: null,
};

const getters = {
  top: (curState) => curState.top,
  height: (curState) => curState.height,
  width: (curState) => curState.width,
  padding: (curState) => curState.padding,
  margin: (curState) => curState.margin,
  mobile: (curState) => curState.mobile,
  orientation: (curState) => curState.orientation,
  touchCell: (curState) => curState.touchCell,
};

const mutations = {
  update(curState) {
    curState.top = window.document.body.offsetTop;
    const navbarEl = document.getElementById('ekagra-navbar');
    if (navbarEl) {
      curState.top += navbarEl.offsetHeight;
    }
    curState.left = window.document.body.offsetLeft;
    curState.height = window.innerHeight - curState.top;
    curState.width = window.innerWidth;
    curState.mobile = Object.hasOwnProperty.call(window, 'ontouchstart');
    curState.orientation = curState.width > curState.height ? 'landscape' : 'portrait';
    curState.margin = curState.mobile ? '1rem' : '2rem';
    curState.padding = curState.mobile ? '1rem' : '2rem';
    const mobileFontHeight = curState.orientation === 'portrait'
      ? `calc( ${curState.height}px / 25)`
      : `calc( ${curState.height}px / 12)`;
    curState.touchCell = curState.mobile
      ? mobileFontHeight
      : '1rem';
  },
};

const actions = {
  // authorize(context, payload) {
};

export default {
  namespaced: true,
  state,
  getters,
  actions,
  mutations,
};
