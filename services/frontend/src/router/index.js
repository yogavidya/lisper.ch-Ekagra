import Vue from 'vue';
import VueRouter from 'vue-router';
import Home from '../views/Home.vue';
import Instructions from '../views/Instructions.vue';
import Access from '../views/Access.vue';
import Confirm from '../views/Confirm.vue';
import CancelCheckout from '../views/CancelCheckout.vue';
import CheckoutOk from '../views/CheckoutOk.vue';
import IPNRecipient from '../views/IPNRecipient.vue';
import Page404 from '../views/404.vue';

Vue.use(VueRouter);

const routes = [
  {
    path: '/',
    name: 'Home',
    component: Home,
    props: true,
  },
  {
    path: '/instructions',
    name: 'Instructions',
    component: Instructions,
    props: true,
  },
  {
    path: '/access',
    name: 'Access',
    component: Access,
  },
  {
    path: '/confirm',
    name: 'Confirm',
    component: Confirm,
  },
  {
    path: '/cancel-checkout',
    name: 'CancelCheckout',
    component: CancelCheckout,
  },
  {
    path: '/checkout-ok',
    name: 'CheckoutOk',
    component: CheckoutOk,
  },
  {
    path: '/ipn-recipient',
    name: 'IPNRecipient',
    component: IPNRecipient,
  },
  {
    path: '/about',
    name: 'About',
    // route level code-splitting
    // this generates a separate chunk (about.[hash].js) for this route
    // which is lazy-loaded when the route is visited.
    component: () => import(/* webpackChunkName: "about" */ '../views/About.vue'),
  },
  { path: '*', component: Page404 },
];

const router = new VueRouter({
  mode: 'history',
  base: process.env.BASE_URL,
  routes,
});

router.beforeEach((to, from, next) => {
  console.debug(`to=${to}, from=${from}`);
  next();
});

export default router;
