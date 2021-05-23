<template>
<div>
    <b-modal
      id="avviso-abbonamento"
      title="Abbonamento riuscito"
      visible
      centered
      no-close-on-backdrop
      hide-header-close
      ok-only
      v-on:hidden="$event.vueTarget.$router.push('/');"
      >
      <p>
        Grazie per l'abbonamento.<br>
        La transazione è andata a buon fine, e PayPal ti ha inviato un'email di conferma.<br>
        Accedi al tuo account PayPal se vuoi vedere i dettagli della transazione.<br>
        Clicca su "Ok" per tornare alla homepage di Ekagra Yoga:<br>
        ora puoi accedere con le tue credenziali.<br>
      </p>
    </b-modal>
  </div>
</template>
<script>
export default {
  name: 'CheckoutOK',
};
</script>
