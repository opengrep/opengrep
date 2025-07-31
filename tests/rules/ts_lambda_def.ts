class Myclass implements CRM {
  private getUserFromEmail =  (email) => {
    // ruleid:test
   const q = `SELECT Id, Email FROM User WHERE Email = '${email}'`;
  };

}
 
