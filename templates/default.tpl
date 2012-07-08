<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title><page-title/></title>
  </head>
  <body>
    <notice/>
    <h1><page-title/></h1>
    <header-comment/>
    <table>
      <attendees/>
    </table>
    <h2>参加登録</h2>
    <form method="post" action="${script-path}new">
      <label>名前 <input name="attendee-name" type="text"/></label><br>
      <label>パスワード <input name="attendee-password" type="text"/></label><br>
      <label>ひとこと <input name="attendee-comment" type="text"/></label><br>
      <button type="submit">送信</button>
    </form>
    <h2>キャンセル</h2>
    <form method="post" action="${script-path}delete">
      <label>Id <input name="attendee-id" type="text"/></label>
      <label>パスワード <input name="attendee-password" type="text"/></label>
      <button type="submit">送信</button>
    </form>
  </body>
</html>
