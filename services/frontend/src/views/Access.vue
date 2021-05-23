<template>
  <div class="login-view">
    <b-form @submit="onSubmit" class="login-form">
      <h2 class="sr-only">Login Form</h2>
      <b-form-group>
        <b-form-input
          v-model="email"
          class="form-control"
          type="email"
          name="email"
          placeholder="Email" />
      </b-form-group>
      <b-form-group v-if="this.$route.query.registration === 'true'">
        <b-form-input
          v-model="nickname"
          class="form-control"
          name="nickname"
          placeholder="nickname" />
      </b-form-group>
      <b-form-group>
        <b-form-input
          v-model="password"
          class="form-control"
          type="password"
          name="password"
          placeholder="Password"
        />
      </b-form-group>
      <div class="form-group">
        <b-button variant="primary" type="submit">
          {{this.$route.query.registration === 'true' ? 'Registrami' : 'Accedi'}}
        </b-button>
      </div>
      <div class="forgot" @click="onForgotPassword">Dimenticato la password?</div>
    </b-form>
    <b-modal
      id="avviso-registrazione"
      title="Registrazione"
      centered
      no-close-on-backdrop
      hide-header-close
      ok-only
      v-on:hidden="$event.vueTarget.$router.push('/');"
      >
      <p>
        I dati sono stati registrati.<br>
        Riceverai un'email da 'non-rispondere-ekagra-yoga@lisper.ch'<br>
        Clicca sul link contenuto nell'email per completare la registrazione.
        Grazie!
      </p>
    </b-modal>
    <b-modal
      id="avviso-errore"
      title="Errore"
      centered
      no-close-on-backdrop
      hide-header-close
      ok-only
      >
      <p>
        La richiesta non è stata accettata:<br>
        <span >{{this.lastError}}</span>
      </p>
    </b-modal>
  </div>
</template>

<script>
export default {
  name: 'Access',
  data() {
    return {
      email: '',
      nickname: '',
      password: '',
      lastError: '',
    };
  },
  methods: {
    onSubmit(evt) {
      evt.preventDefault();
      if (this.$route.query.registration === 'true') {
        this.$store.dispatch('auth/register', {
          email: this.email,
          nickname: this.nickname,
          password: this.password,
        })
          .then(() => {
            this.$bvModal.show('avviso-registrazione');
          })
          .catch((err) => {
            this.lastError = err.toString();
            this.$bvModal.show('avviso-errore');
          });
      } else {
        this.$store.dispatch('auth/authorize', {
          email: this.email,
          password: this.password,
        })
          .then(() => {
            this.$root.$router.push('/');
          })
          .catch((err) => {
            this.lastError = err.toString();
            this.$bvModal.show('avviso-errore');
          });
      }
    },
    onForgotPassword() {
      window.alert(`Non implementato ancora, scusate`);
    },
  },
};
</script>

<style lang="scss" scoped>
.login-view {
  background-color: var(--ekagra-background);
  height: 80vh;
  background-size: cover;
  padding-top: 20vh;
  padding-bottom: 20vh;
  padding-left: 25vw;
  padding-right: 25vw;
}
.login-form {
  width: 50vw;
  height: 40vh;
}
.forgot {
  text-shadow: 2px 2px black;
  color: floralwhite;
  cursor: pointer,
}
</style>
