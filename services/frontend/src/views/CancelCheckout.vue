<template>
<div>
    <b-modal
      id="avviso-annullamento"
      title="Annullamento"
      visible
      centered
      no-close-on-backdrop
      hide-header-close
      ok-only
      v-on:hidden="$event.vueTarget.$router.push('/');"
      >
      <p>
        Il pagamento è stato annullato.</p>
      <p v-if="pending && pendingDeleted">
        Le tue informazioni di registrazione sono state cancellate.</p>
      <p v-else>
        Cancelleremo le tue informazioni di registrazione entro 24 ore.</p>
      <p>Clicca su "Ok" per tornare alla homepage di Ekagra Yoga.</p>
    </b-modal>
  </div>
</template>

<script>
import http from '@/common/http';

export default {
  data() {
    return {
      pending: null,
      pendingDeleted: false,
      pendingDeleteError: null,
    };
  },
  mounted() {
    if (this.$route.params.pending) {
      this.pending = this.$route.params.pending;
      http.post('delete-unsubscribed-user', {
        pending: this.pending,
      })
        .then(() => {
          this.pendingDeleted = true;
        })
        .catch((err) => {
          this.pendingDeleteError = err;
        });
    }
  },
};
</script>
