<template>
  <footer>
    <a href="/">Codeforces</a> &copy; 2021 by Mike Mirzayanov
    <div class="userStat">
      Number of users: {{ usersNumber }}
    </div>
    <div class="postStat">
      Number of posts: {{ postsNumber }}
    </div>
  </footer>
</template>

<script>
export default {
  name: "Footer",
  props: ["postsNumber", "usersNumber"]
}
</script>

<style scoped>

</style>