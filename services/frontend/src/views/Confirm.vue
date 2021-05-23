<template>
<div>
  <div id="confirm-text" :style="{
        'margin-top': margin,
      }">
      Registrazione in corso. Attendi...
  </div>
      <b-modal
        id="avviso-errore"
        title="Errore"
        centered
        no-close-on-backdrop
        hide-header-close
        ok-only
        v-on:hidden="window.setTimeout(() => $event.vueTarget.$router.push('/'), 2000);"
        >
        <p>
          La richiesta non è stata accettata:<br>
          <span >{{this.lastError}}</span>
        </p>
      </b-modal>
    <div id="subscribe-div" class="hidden" :style="{
        'margin-top': margin,
        padding: padding,
      }">
      La registrazione è stata completata correttamente. <br/>
      Nota che la tua email verrà usata <strong>solo</strong>
       per identificarti quando accedi a Ekagra Yoga. <br/>
      Nessun altro uso verrà mai fatto della tua email o di qualsiasi informazione su di te. <br/>
      Per potere accedere a tutto il materiale dei corsi di Ekagra Yoga, <br/>
      <strong>clicca su questo bottone</strong> per autorizzare il pagamento mensile su PayPal:
    </div>
    <Paypal />
    <br>Nota che l'abbonamento si può disdire in qualsiasi momento:
    <br>basta cancellare il pagamento mensile dal tuo account PayPal.
    <br/>Se invece sei arrivato qui per errore o hai cambiato idea, clicca qui:
    <button class="btn btn-danger" @click="onClickAnnulla">Annulla</button>
    e la tua registrazione verrà cancellata.
  </div>
</template>

<script>
import http from '@/common/http';
import Paypal from '@/components/Paypal.vue';

export default {
  name: 'Confirm',
  components: {
    Paypal,
  },
  data() {
    return {
      pending: null,
      lastError: '',
    };
  },
  mounted() {
    this.pending = this.$root.$route.query.pending;
    http.post('confirm', {
      pending: this.pending,
    })
      .then(() => {
        document.getElementById('confirm-text').innerHTML = 'Registrazione completa.';
        document.getElementById('subscribe-div').classList.remove('hidden');
      })
      .catch((err) => {
        this.lastError = err.toString();
        this.$bvModal.show('avviso-errore');
      });
  },
  methods: {
    onClickAnnulla() {
      this.$router.push({
        name: 'CancelCheckout',
        params: {
          pending: this.pending,
        },
      });
    },
  },
  computed: {
    margin() {
      return this.$store.getters['document/margin'];
    },
    padding() {
      return this.$store.getters['document/padding'];
    },
  },
};
</script>
