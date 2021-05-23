<template>
  <div id="app">
    <div v-if="!fullScreenMode" id="nav" style="padding-bottom: 0;">
      <Navbar/>
    </div>
    <router-view
      :fullScreenMode="fullScreenMode"
      :auth="auth"
      @full-screen-mode='onFullScreenModeChange($event)'
      @content-update="sendContentTreeRequest();"/>
  </div>
</template>

<script>
import Navbar from '@/components/Navbar.vue';
import http from '@/common/http';

export default {
  name: 'App',
  components: {
    Navbar,
  },
  mounted() {
    document.body.style.overflow = 'auto';
    this.updateLayoutParameters();
    window.addEventListener('resize', () => {
      this.updateLayoutParameters();
    });
    window.document.addEventListener('focusin',
      (e) => {
        const closest = (el, fn) => (el && (fn(el) ? el : closest(el.parentNode, fn)));
        const hasMceParent = (el) => {
          if (!el.classList) return false;
          if (el.classList.contains('tox-tinymce-aux')
          || el.classList.contains('moxman-window')
          || el.classList.contains('tam-assetmanager-root')) {
            return true;
          }
          return false;
        };
        if (closest(e.target, hasMceParent)) {
          e.stopImmediatePropagation();
        }
      });
    this.sendContentTreeRequest();
  },
  data() {
    return {
      fullScreenMode: false,
    };
  },
  computed: {
    auth() {
      return this.$store.getters['auth/isAuthorized'];
    },
  },
  watch: {
    auth() {
      this.sendContentTreeRequest();
    },
  },
  methods: {
    updateLayoutParameters() {
      this.$store.commit('document/update');
    },
    sendContentTreeRequest() {
      http.get('content-menu-data', {
        headers: {
          lisperClient: 'true',
          Authorization: `Bearer ${this ? this.$store.getters['auth/token'] : null}`,
        },
      })
        .then((response) => {
          const newTreeData = JSON.parse(response.data);
          this.$store.commit('setTreeMenuData', newTreeData);
        })
        .catch((err) => {
          this.$store.commit('setTreeMenuData', {
            root: {
              titolo: err.toString(),
            },
          });
        });
    },
    onFullScreenModeChange(state) {
      console.debug(state);
      this.fullScreenMode = state;
      this.$store.commit('document/update');
    },
  },
};
</script>

<style lang="scss">

#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  --ekagra-background: #596e8b;
  --ekagra-text-background: #f0f0f0;
}
@media screen and (max-width: 360px) {
  h1 {
    font-size: 150% !important;
  }
  h2 {
    font-size: 130% !important;
  }
  h3 {
    font-size: 110% !important;
  }
}
@media screen and (max-width: 720px) and (min-width: 361px) {
  h1 {
    font-size: 200% !important;
  }
  h2 {
    font-size: 150% !important;
  }
  h3 {
    font-size: 130% !important;
  }
}
@media screen and (min-width: 721px) {
}
.invisible {
  opacity: 0;
}
invisible:hover {
  cursor: pointer;
}
</style>
