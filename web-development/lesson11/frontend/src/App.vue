<template>
  <div id="app">
    <Header :user="user"/>
    <Middle :posts="posts" :users="users" :comments="filterComments" :commentsSizes="commentsSizes"/>
    <Footer/>
  </div>
</template>

<script>
import Header from "./components/Header";
import Middle from "./components/Middle";
import Footer from "./components/Footer";
import axios from "axios"

export default {
  name: 'App',
  components: {
    Footer,
    Middle,
    Header
  },
  data: function () {
    return {
      user: null,
      post: null,
      posts: [],
      users: [],
      comments: {},
      commentsSizes: {}
    }
  },
  beforeMount() {
    if (localStorage.getItem("jwt") && !this.user) {
      this.$root.$emit("onJwt", localStorage.getItem("jwt"));
    }

    axios.get("/api/1/posts").then(response => {
      this.posts = response.data;
    });

    axios.get("/api/1/users").then(response => {
      this.users = response.data;
    });

    axios.get("/api/1/comments").then(response => {
      this.comments = response.data;
    });
  },
  computed: {
    filterComments: function () {
      if (this.post === null) {
        return {};
      }
      return this.comments.filter(c => c.post.id === this.post.id);
    }
  },
  beforeCreate() {
    this.$root.$on("countCommentsSizes", () => {
      Object.values(this.posts).forEach(post => {
        this.commentsSizes[post.id] = 0;
      })
      Object.values(this.comments).forEach(comment => {
        ++this.commentsSizes[comment.post.id];
      })
    });

    this.$root.$on("onEnter", (login, password) => {
      if (password === "") {
        this.$root.$emit("onEnterValidationError", "Password is required");
        return;
      }

      axios.post("/api/1/jwt", {
        login, password
      }).then(response => {
        localStorage.setItem("jwt", response.data);
        this.$root.$emit("onJwt", response.data);
      }).catch(error => {
        this.$root.$emit("onEnterValidationError", error.response.data);
      });
    });

    this.$root.$on("onJwt", (jwt) => {
      localStorage.setItem("jwt", jwt);

      axios.get("/api/1/users/auth", {
        params: {
          jwt
        }
      }).then(response => {
        this.user = response.data;
        this.$root.$emit("onChangePage", "Index");
      }).catch(() => this.$root.$emit("onLogout"))
    });

    this.$root.$on("onLogout", () => {
      localStorage.removeItem("jwt");
      this.user = null;
    });

  }
}
</script>

<style>
#app {

}
</style>
