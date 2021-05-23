<template>
<div id="header">
  <b-navbar id="ekagra-navbar"
    fixed="top"
    toggleable="sm"
    sticky>
    <b-navbar-brand to="/" id="ekagra-brand">
      <div class="d-flex flex-row" style="margin-right: 2rem">
        <img class="navbar-logo" src="../assets/logo-ekagra.png"
        @click.middle.stop.prevent="onSecretFunction">
        <div class="navbar-logo"/>
        <a class="titolo">Ekagra Yoga online</a>
      </div>
    </b-navbar-brand>
    <b-navbar-toggle target="ekagra-navbar-links"/>
    <b-collapse is-nav id="ekagra-navbar-links">
    <b-navbar-nav>
      <b-nav-item to="/">Home</b-nav-item>
      <b-nav-item to="About">Chi sono</b-nav-item>
      <b-nav-item to="Instructions">Come funziona</b-nav-item>
      <!-- Navbar dropdowns -->
      <b-nav-item-dropdown :text="this.stringaMenuAccesso" right>
        <b-dropdown-item
          v-if="!this.logged"
          to="access?registration=true">
            Registrazione
        </b-dropdown-item>
        <b-dropdown-item
          v-if="!this.logged"
          to="access">
            Accedi...
        </b-dropdown-item>
        <b-dropdown-item
          v-if="this.logged"
          v-on:click="logOut();">
            Chiudi la sessione
        </b-dropdown-item>
      </b-nav-item-dropdown>
    </b-navbar-nav>
    </b-collapse>
  </b-navbar>
</div>
</template>

<script>
export default {
  name: 'Navbar',
  props: {
    msg: String,
  },
  computed: {
    logged() {
      return this.$store.getters['auth/isAuthorized'];
    },
    stringaMenuAccesso() {
      if (!this.logged) {
        return 'Accesso';
      }
      return this.$store.getters['auth/nickname'];
    },
  },
  methods: {
    logOut() {
      this.$store.dispatch('auth/closeSession');
    },
    onLoadBanner() {
      const picElement = document.getElementById('banner');
      const boxElement = document.getElementById('header');
      if (picElement) {
        picElement.style.height = `${picElement.offsetHeight}px`;
        boxElement.style.height = `${
          picElement.offsetHeight - boxElement.offsetTop
        }px`;
        // this.$store.commit('setVerticalStart', picElement.offsetHeight);
        this.$store.commit('document/update');
      }
    },
    onSecretFunction() {
      console.debug('Alive and kicking!');
    },
  },
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped lang="scss">

$banner-url: url("/Ekagra/banners/banner-lago-3.jpg");

@media screen and (max-width: 360px) {
  #ekagra-navbar {
    ::parent {
      padding: 4px;
    }
    opacity: 1;
    // background-color: #bbb;
    background-image: $banner-url;
    image-rendering: optimizeSpeed;
    background-size: cover;
    width: 100vw;
    height: 10vh;
    box-shadow: 5px 5px 5px 0px rgba(0,0,0,0.75);
        a {
      font-weight: bold;
      color: #2c3e50;

      &.router-link-exact-active {
        color: #42b983;
      }
    }
  }

  #ekagra-brand {
    display: inline-flex;
    float: left;
  }

  .navbar-logo {
    max-height: 5vh; //48px;
    margin-right: 1rem;
    flex-grow: 0;
    flex-shrink: 0;
  }

  .titolo {
    display: none;
    align-self: center;
    text-shadow: 1px 1px 2px #479681;
  }

  #ekagra-navbar-links {
    opacity: 1;
    background-color: rgb(89, 110, 139);
    box-shadow: 5px 5px 5px 0px rgba(0,0,0,0.75);
  }

}


@media screen and (max-width: 720px) and (min-width: 361px) {
  #ekagra-navbar {
    ::parent {
      padding: 4px;
    }
    opacity: 1;
    // background-color: #bbb;
    background-image: $banner-url;
    image-rendering: optimizeSpeed;
    background-size: cover;
    width: 100vw;
    height: 10vh;
    box-shadow: 5px 5px 5px 0px rgba(0,0,0,0.75);
        a {
      font-weight: bold;
      color: #2c3e50;

      &.router-link-exact-active {
        color: #42b983;
      }
    }
  }

  #ekagra-brand {
    display: inline-flex;
    float: left;
  }

  .navbar-logo {
    max-height: 5vh; //48px;
    margin-right: 1rem;
    flex-grow: 0;
    flex-shrink: 0;
  }

  .titolo {
    display: none;
    align-self: center;
    text-shadow: 1px 1px 2px #479681;
  }

}

@media screen and (min-width: 721px) {
  #ekagra-navbar {
    ::parent {
      padding: 8px;
    }
    opacity: 1;
    // background-color: #bbb;
    background-image: $banner-url;
    image-rendering: optimizeSpeed;
    background-size: cover;
    width: 100vw;
    height: 20vh;
    box-shadow: 5px 5px 5px 0px rgba(0,0,0,0.75);
  }

  #ekagra-brand {
    display: inline-flex;
    float: left;
  }

  .navbar-logo {
    max-height: 10vh; //48px;
    margin-right: 1rem;
    flex-grow: 0;
    flex-shrink: 0;
  }
}

.titolo {
  align-self: center;
  color: yellow;
  text-shadow: 1px 1px 2px #479681;
}

#navbar:hover {
  opacity: .7;
}
</style>
